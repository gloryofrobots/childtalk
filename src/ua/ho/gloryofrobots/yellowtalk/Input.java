package ua.ho.gloryofrobots.yellowtalk;

public class Input {
	public final int INPUT_EOF = '\0';

	private class Marker {
		Marker(int _pos, int _line, int _column) {
			position = _pos;
			line = _line;
			column = _column;
		}

		int position;
		int line;
		int column;

	};

	char[] m_text;

	int m_position; /* current index into text */

	int m_size; /* total number of chars in text */
	int m_line; /* current line number, starting from 1 */
	int m_column; /* current column number, starting from 1 */

	Marker m_marker;

	Input(String data) {
		m_text = data.toCharArray();
		m_size = m_text.length;
		m_line = 1;
		m_column = 1;
	}

	String filter_double_bangs(String chunk) {
		char[] p = chunk.toCharArray();
		int size = chunk.length();

		if (size < 2)
			return new String(chunk);
		
		int i = 0;
		char ch;
		String result = "";
		/* count number of redundant bangs */
		while (i < size) {

			if (p[i] == '!' && i < (size - 1)) {

				ch = p[i + 1];
				while (ch == '!') {
					i++;
					ch = p[i + 1];
				}
				result += '!';
			} else {
				result += p[i];
			}
		}

		return result;
	}

	// ////////////////////////////////////////////////////
	char lookAhead(int i) {
		// GlobalProvider::get()->getErrorControl()->error("");

		if (i == 0) {
			return 0x0000;
		}

		if (i < 0) {
			i++;
			if ((m_position + i - 1) < 0) {
				return INPUT_EOF;
			}
		}

		if ((m_position + i - 1) >= m_size) {
			return INPUT_EOF;
		}

		return m_text[m_position + i - 1];
	}

	// ////////////////////////////////////////////////////
	int getLine() {
		return m_line;
	}

	// ////////////////////////////////////////////////////
	int getColumn() {
		return m_column;
	}

	// ////////////////////////////////////////////////////
	void mark() {
		m_marker.position = m_position;
		m_marker.line = m_line;
		m_marker.column = m_column;
	}

	// ////////////////////////////////////////////////////
	void rewind() {
		seek(m_marker.position);
		m_line = m_marker.line;
		m_column = m_marker.column;
	}

	// ////////////////////////////////////////////////////
	void seek(int index) {
		if (index <= m_position) {
			m_position = index;
		}

		while (m_position < index) {
			consume();
		}
	}

	// ////////////////////////////////////////////////////
	void consume() {
		if (m_position >= m_size) {
			return;
		}

		m_column++;

		/* FIXME 0x000A is newline */
		if (m_text[m_position] == 0x000A) {
			m_line++;
			m_column = 1;
		}

		m_position++;
	}

	// ////////////////////////////////////////////////////
	int size() {
		return m_size;
	}

	// ////////////////////////////////////////////////////
	int index() {
		return m_position;
	}

	// ////////////////////////////////////////////////////
	String range(int start, int end) {
		int len = end - start;

		char[] buf = new char[len + 1];
		for (int i = 0; i < len; ++i) {
			buf[i] = m_text[start + i];
		}

		return new String(buf);
	}

	// ////////////////////////////////////////////////////
	String nextChunk() {
		while (lookAhead(1) != INPUT_EOF) {

			if (lookAhead(1) != '!') {
				consume();
				continue;
			}

			/* skip past doubled bangs */
			if (lookAhead(1) == '!' && lookAhead(2) == '!') {
				consume();
				consume();
				continue;
			}

			int start = index();
			String chunk = range(start, index());
			String chunk_filtered = filter_double_bangs(chunk);

			consume();

			return chunk_filtered;
		}

		return null;
	}
}
