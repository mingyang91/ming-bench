//! AST-based quality gate checks.
//!
//! Parses Rust source files with `syn` and rejects patterns that violate
//! quality-gate rules. More robust than grep — ignores comments and strings.

use std::path::Path;
use syn::visit::Visit;

/// A single violation found by the AST checker.
#[derive(Debug)]
pub struct Violation {
    pub file: String,
    pub line: usize,
    pub message: String,
}

/// Walk all `.rs` files under `src_dir` (excluding tests/) and check for violations.
pub fn check_ast_rules(src_dir: &Path) -> Vec<Violation> {
    let mut violations = Vec::new();
    visit_rs_files(src_dir, &mut violations);
    violations
}

fn visit_rs_files(dir: &Path, violations: &mut Vec<Violation>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            // Skip tests directory — those are ours, not agent code
            if path.file_name().is_some_and(|n| n == "tests") {
                continue;
            }
            visit_rs_files(&path, violations);
        } else if path.extension().is_some_and(|e| e == "rs") {
            check_file(&path, violations);
        }
    }
}

fn check_file(path: &Path, violations: &mut Vec<Violation>) {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return,
    };
    let file = match syn::parse_file(&content) {
        Ok(f) => f,
        Err(_) => return, // parse errors will be caught by cargo build
    };

    let file_str = path.display().to_string();
    let mut checker = RuleChecker {
        file: &file_str,

        violations,
    };
    checker.visit_file(&file);
}

struct RuleChecker<'a> {
    file: &'a str,
    violations: &'a mut Vec<Violation>,
}

impl<'a> RuleChecker<'a> {
    fn line_of(&self, span: proc_macro2::Span) -> usize {
        span.start().line
    }

    /// Check if a type path ends with `String` (i.e., the error type is String).
    fn is_string_type(ty: &syn::Type) -> bool {
        if let syn::Type::Path(tp) = ty {
            if let Some(seg) = tp.path.segments.last() {
                return seg.ident == "String";
            }
        }
        false
    }

    /// Check if a type is `Result<_, String>`.
    fn is_result_string(ty: &syn::Type) -> bool {
        if let syn::Type::Path(tp) = ty {
            if let Some(seg) = tp.path.segments.last() {
                if seg.ident == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        // Result has 2 type args; the second is the error type
                        let type_args: Vec<_> = args
                            .args
                            .iter()
                            .filter_map(|a| {
                                if let syn::GenericArgument::Type(t) = a {
                                    Some(t)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if type_args.len() == 2 {
                            return Self::is_string_type(type_args[1]);
                        }
                    }
                }
            }
        }
        false
    }
}

impl<'ast, 'a> Visit<'ast> for RuleChecker<'a> {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        // Check return type for Result<_, String>
        if let syn::ReturnType::Type(_, ty) = &node.sig.output {
            if Self::is_result_string(ty) {
                self.violations.push(Violation {
                    file: self.file.to_string(),
                    line: self.line_of(node.sig.ident.span()),
                    message: format!(
                        "fn `{}` returns Result<_, String> — use a typed error enum instead",
                        node.sig.ident
                    ),
                });
            }
        }
        // Continue visiting nested items
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
        // Check return type for Result<_, String> on impl methods
        if let syn::ReturnType::Type(_, ty) = &node.sig.output {
            if Self::is_result_string(ty) {
                self.violations.push(Violation {
                    file: self.file.to_string(),
                    line: self.line_of(node.sig.ident.span()),
                    message: format!(
                        "method `{}` returns Result<_, String> — use a typed error enum instead",
                        node.sig.ident
                    ),
                });
            }
        }
        syn::visit::visit_impl_item_fn(self, node);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_source(src: &str) -> Vec<Violation> {
        let file = syn::parse_file(src).unwrap();
        let mut violations = Vec::new();
        let mut checker = RuleChecker {
            file: "test.rs",
            violations: &mut violations,
        };
        checker.visit_file(&file);
        violations
    }

    #[test]
    fn rejects_result_string_return() {
        let v = check_source("fn foo() -> Result<i32, String> { Ok(1) }");
        assert_eq!(v.len(), 1);
        assert!(v[0].message.contains("Result<_, String>"));
    }

    #[test]
    fn accepts_result_eval_error() {
        let v = check_source("fn foo() -> Result<i32, EvalError> { Ok(1) }");
        assert!(v.is_empty());
    }

    #[test]
    fn rejects_impl_method_result_string() {
        let v = check_source(
            "struct S; impl S { fn bar(&self) -> Result<(), String> { Ok(()) } }",
        );
        assert_eq!(v.len(), 1);
        assert!(v[0].message.contains("bar"));
    }

    #[test]
    fn accepts_string_in_ok_position() {
        let v = check_source("fn foo() -> Result<String, EvalError> { todo!() }");
        assert!(v.is_empty());
    }
}
