pub const MULTICHAR_OPERATORS: &'static [&'static str] = &["==", "!=", "->"];

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub text: &'a str,
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

impl<'a> Token<'a> {
    pub fn dummy() -> Self {
        Self {
            text: "",
            kind: TokenKind::Token,
            start: 0,
            end: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Token,
    Alphanumeric,
    Comment,
}

pub fn lex(src: &str) -> Lex {
    Lex {
        src,
        offset: 0,
        ignore_comments: true,
    }
}

pub struct Lex<'a> {
    src: &'a str,
    offset: usize,
    ignore_comments: bool,
}

impl<'a> Lex<'a> {
    fn next_token(&mut self) -> Option<Token<'a>> {
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
        } else if let Some((j, text)) = any_keyword_of(MULTICHAR_OPERATORS)(i) {
            // Multichar operator
            (j, text, TokenKind::Token)
        } else if let Some((j, text)) = pair(keyword("//"), take_to(|ch| ch == '\n'))(i) {
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

impl<'a> Iterator for Lex<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = self.next_token()?;
        while self.ignore_comments && token.kind == TokenKind::Comment {
            token = self.next_token()?;
        }
        Some(token)
    }
}

fn any_keyword_of(keywords: &'static [&'static str]) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |i| {
        for kw in keywords {
            let result = keyword(kw)(i);
            if result.is_some() {
                return result;
            }
        }
        None
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

fn take_to(pred: impl Fn(char) -> bool) -> impl Fn(&str) -> Option<(&str, &str)> {
    take_with_predicate(pred, true)
}

// fn take_until(pred: impl Fn(char) -> bool) -> impl Fn(&str) -> Option<(&str, &str)> {
//     take_with_predicate(pred, false)
// }

fn take_while(pred: impl Fn(char) -> bool) -> impl Fn(&str) -> Option<(&str, &str)> {
    take_with_predicate(move |c| !pred(c), false)
}

fn take_with_predicate(
    pred: impl Fn(char) -> bool,
    include_first_non_match: bool,
) -> impl Fn(&str) -> Option<(&str, &str)> {
    move |i| {
        let end_indices = i
            .char_indices()
            .map(|(idx, _)| idx)
            .chain(Some(i.len()))
            .skip(1);
        let char_end_indices = end_indices.zip(i.chars());

        let mut included_end_idx = 0;
        let mut excluded_end_idx = 0;

        for (idx, ch) in char_end_indices {
            included_end_idx = idx;
            if pred(ch) {
                break;
            }
            excluded_end_idx = idx;
        }

        let end_idx = if include_first_non_match {
            included_end_idx
        } else {
            excluded_end_idx
        };

        let token = &i[..end_idx];
        let next_i = &i[end_idx..];
        if token.is_empty() {
            None
        } else {
            Some((next_i, token))
        }
    }
}

fn is_whitespace_char(ch: char) -> bool {
    ch.is_whitespace()
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
