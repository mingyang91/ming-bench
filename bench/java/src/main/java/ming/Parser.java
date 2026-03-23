package ming;

import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final List<Tokenizer.Token> tokens;
    private int pos;

    public Parser(List<Tokenizer.Token> tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }

    public List<SchemeValue> parseAll() throws EvalError {
        List<SchemeValue> exprs = new ArrayList<>();
        while (peek().type() != Tokenizer.TokenType.EOF) {
            exprs.add(parseExpr());
        }
        return exprs;
    }

    private SchemeValue parseExpr() throws EvalError {
        Tokenizer.Token tok = peek();
        return switch (tok.type()) {
            case QUOTE -> {
                advance();
                SchemeValue quoted = parseExpr();
                yield new SchemeValue.ListVal(List.of(new SchemeValue.SymbolVal("quote"), quoted));
            }
            case LPAREN -> parseList();
            case INTEGER -> { advance(); yield new SchemeValue.IntVal(Long.parseLong(tok.value())); }
            case BOOLEAN -> { advance(); yield new SchemeValue.BoolVal(tok.value().equals("true")); }
            case STRING -> { advance(); yield new SchemeValue.StringVal(tok.value()); }
            case SYMBOL -> { advance(); yield new SchemeValue.SymbolVal(tok.value()); }
            case RPAREN -> throw new EvalError("Unexpected )");
            case EOF -> throw new EvalError("Unexpected end of input");
        };
    }

    private SchemeValue parseList() throws EvalError {
        advance(); // skip (
        List<SchemeValue> elements = new ArrayList<>();
        while (peek().type() != Tokenizer.TokenType.RPAREN) {
            if (peek().type() == Tokenizer.TokenType.EOF) {
                throw new EvalError("Unterminated list");
            }
            elements.add(parseExpr());
        }
        advance(); // skip )
        return new SchemeValue.ListVal(elements);
    }

    private Tokenizer.Token peek() {
        return tokens.get(pos);
    }

    private Tokenizer.Token advance() {
        return tokens.get(pos++);
    }
}
