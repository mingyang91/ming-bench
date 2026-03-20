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
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        let is_tests_dir = path.is_dir() && path.file_name().is_some_and(|n| n == "tests");
        if path.is_dir() && !is_tests_dir {
            visit_rs_files(&path, violations);
        } else if path.extension().is_some_and(|e| e == "rs") {
            check_file(&path, violations);
        }
    }
}

fn check_file(path: &Path, violations: &mut Vec<Violation>) {
    let Ok(content) = std::fs::read_to_string(path) else { return };
    let Ok(file) = syn::parse_file(&content) else { return };

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

    fn check_return_type(&mut self, sig: &syn::Signature) {
        let syn::ReturnType::Type(_, ty) = &sig.output else { return };
        if !Self::is_result_string(ty) {
            return;
        }
        self.violations.push(Violation {
            file: self.file.to_string(),
            line: self.line_of(sig.ident.span()),
            message: format!(
                "fn `{}` returns Result<_, String> — use a typed error enum instead",
                sig.ident
            ),
        });
    }

    /// Check if a type path ends with `String` (i.e., the error type is String).
    fn is_string_type(ty: &syn::Type) -> bool {
        let syn::Type::Path(tp) = ty else { return false };
        tp.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "String")
    }

    /// Check if a type is `Result<_, String>`.
    fn is_result_string(ty: &syn::Type) -> bool {
        let syn::Type::Path(tp) = ty else { return false };
        let Some(seg) = tp.path.segments.last() else { return false };
        if seg.ident != "Result" {
            return false;
        }
        let syn::PathArguments::AngleBracketed(args) = &seg.arguments else {
            return false;
        };
        let type_args: Vec<_> = args
            .args
            .iter()
            .filter_map(extract_generic_type)
            .collect();
        type_args.len() == 2 && Self::is_string_type(type_args[1])
    }
}

fn extract_generic_type(arg: &syn::GenericArgument) -> Option<&syn::Type> {
    if let syn::GenericArgument::Type(t) = arg {
        Some(t)
    } else {
        None
    }
}

impl<'ast, 'a> Visit<'ast> for RuleChecker<'a> {
    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        // Check return type for Result<_, String>
        self.check_return_type(&node.sig);
        // Continue visiting nested items
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
        // Check return type for Result<_, String> on impl methods
        self.check_return_type(&node.sig);
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
