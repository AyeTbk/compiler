#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Token,
    Alphanumeric,
    Comment,
}

pub fn lex(src: &str) -> Lex {
    Lex { src, offset: 0 }
}

pub struct Lex<'a> {
    src: &'a str,
    offset: usize,
}

impl<'a> Iterator for Lex<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace
        let i = if let Some((j, whitespace)) = take_while(is_whitespace_char)(self.src) {
            self.offset += whitespace.len();
            j
        } else {
            self.src
        };

        let (next_i, text, kind) = if let Some((j, text)) = take_while(is_identifier_char)(i) {
            // Identifier or number literal
            (j, text, TokenKind::Alphanumeric)
        } else if let Some((j, text)) = pair(keyword("//"), take_until(|ch| ch == '\n'))(i) {
            // Single line comment
            (j, text, TokenKind::Comment)
        } else if let Some((j, text)) = take_one(i) {
            // Token character
            (j, text, TokenKind::Token)
        } else {
            return None;
        };

        let start = self.offset;
        let end = start + text.len();
        self.offset = end;
        self.src = next_i;

        Some(Token {
            text,
            kind,
            start,
            end,
        })
    }
}

fn pair(
    f: impl Fn(&str) -> Option<(&str, &str)>,
    g: impl Fn(&str) -> Option<(&str, &str)>,
) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |i| {
        let (j, _) = f(i)?;
        let (k, _) = g(j)?;
        let end_idx = i.len() - k.len();
        let fg_token = &i[..end_idx];
        let next_i = k;
        Some((next_i, fg_token))
    }
}

fn keyword(kw: &'static str) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |i| {
        if i.len() < kw.len() {
            None
        } else {
            let i_kw = &i[..kw.len()];
            if i_kw == kw {
                let next_i = &i[kw.len()..];
                Some((next_i, i_kw))
            } else {
                None
            }
        }
    }
}

fn take_one(i: &str) -> Option<(&str, &str)> {
    if i.is_empty() {
        None
    } else {
        let next_i = &i[1..];
        let token = &i[..1];
        Some((next_i, token))
    }
}

fn take_until(pred: impl Fn(char) -> bool) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |i| {
        let end_indices = i
            .char_indices()
            .map(|(idx, _)| idx)
            .chain(Some(i.len()))
            .skip(1);
        let char_end_indices = end_indices.zip(i.chars());

        let mut end_idx = 0;

        for (idx, ch) in char_end_indices {
            if pred(ch) {
                break;
            }
            end_idx = idx;
        }
        let token = &i[..end_idx];
        let next_i = &i[end_idx..];
        if token.is_empty() {
            None
        } else {
            Some((next_i, token))
        }
    }
}

fn take_while(pred: impl Fn(char) -> bool) -> impl Fn(&str) -> Option<(&str, &str)> {
    take_until(move |ch| !pred(ch))
}

fn is_whitespace_char(ch: char) -> bool {
    ch.is_whitespace()
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
