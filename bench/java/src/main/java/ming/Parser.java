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
                yield new SchemeValue.ListVal(List.of(new SchemeValue.SymbolVal("quote", tok.line(), tok.col()), quoted), tok.line(), tok.col());
            }
            case LPAREN -> parseList();
            case INTEGER -> { advance(); yield new SchemeValue.IntVal(Long.parseLong(tok.value())); }
            case RATIONAL -> {
                advance();
                String[] parts = tok.value().split("/");
                yield SchemeValue.rational(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
            }
            case DOUBLE -> { advance(); yield new SchemeValue.DoubleVal(Double.parseDouble(tok.value())); }
            case BOOLEAN -> { advance(); yield new SchemeValue.BoolVal(tok.value().equals("true")); }
            case STRING -> { advance(); yield new SchemeValue.StringVal(tok.value()); }
            case CHAR -> { advance(); yield new SchemeValue.CharVal(tok.value().charAt(0)); }
            case SYMBOL -> { advance(); yield new SchemeValue.SymbolVal(tok.value(), tok.line(), tok.col()); }
            case RPAREN -> throw new EvalError("Unexpected ) at " + tok.line() + ":" + tok.col());
            case EOF -> throw new EvalError("Unexpected end of input at " + tok.line() + ":" + tok.col());
        };
    }

    private SchemeValue parseList() throws EvalError {
        Tokenizer.Token open = advance(); // skip (
        List<SchemeValue> elements = new ArrayList<>();
        while (peek().type() != Tokenizer.TokenType.RPAREN) {
            if (peek().type() == Tokenizer.TokenType.EOF) {
                throw new EvalError("Unterminated list at " + open.line() + ":" + open.col());
            }
            elements.add(parseExpr());
        }
        advance(); // skip )
        return new SchemeValue.ListVal(elements, open.line(), open.col());
    }

    private Tokenizer.Token peek() {
        return tokens.get(pos);
    }

    private Tokenizer.Token advance() {
        return tokens.get(pos++);
    }
}
