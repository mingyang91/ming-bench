package ming;

import java.util.ArrayList;

/**
 * An ArrayList that also carries source position (line:col) from the parser.
 */
class SourceList extends ArrayList<Object> {
    final int line;
    final int col;

    SourceList(int line, int col) {
        this.line = line;
        this.col = col;
    }
}
