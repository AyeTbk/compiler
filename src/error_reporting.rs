use crate::serialize::convert::error::Error as ConvertError;
use crate::serialize::parse::error::Error as ParseError;

pub fn make_parse_error_report<'a>(
    parse_error: ParseError<'a>,
    source_name: &'a str,
    source: &'a str,
) -> ErrorReport {
    // FIXME share this part (acquiring source line) in all convert Fns
    let mut source_line = "<bad line>";
    let mut line_number = 1;
    let mut line_start_index;
    let mut line_end_index = 0;
    let mut from = 0;
    let mut to = 0;
    for (line_index, line) in source.split_inclusive('\n').enumerate() {
        line_start_index = line_end_index;
        line_end_index += line.len();

        if parse_error.from < line_end_index {
            source_line = source[line_start_index..line_end_index].trim_end();
            line_number = line_index + 1;
            from = parse_error.from - line_start_index;
            to = parse_error.to - line_start_index;
            break;
        }
    }

    let message = format!("{:?}", parse_error.kind);

    ErrorReport {
        source_name: source_name.to_string(),
        source_line: source_line.to_string(),
        line_number,
        from,
        to,
        message,
    }
}

pub fn make_convert_error_report(
    convert_error: ConvertError,
    source_name: &str,
    source: &str,
) -> ErrorReport {
    // FIXME share this part (acquiring source line) in all convert Fns
    let mut source_line = "<bad line>";
    let mut line_number = 1;
    let mut line_start_index;
    let mut line_end_index = 0;
    let mut from = 0;
    let mut to = 0;
    for (line_index, line) in source.split_inclusive('\n').enumerate() {
        line_start_index = line_end_index;
        line_end_index += line.len();

        if convert_error.from < line_end_index {
            source_line = source[line_start_index..line_end_index].trim_end();
            line_number = line_index + 1;
            from = convert_error.from - line_start_index;
            to = convert_error.to - line_start_index;
            break;
        }
    }

    ErrorReport {
        source_name: source_name.to_string(),
        source_line: source_line.to_string(),
        line_number,
        from,
        to,
        message: convert_error.message,
    }
}

pub fn report_error(error: ErrorReport) {
    // example output:
    //    error: Unexpected { expected: Opcode, got: "?" }
    //     at data/errors.ayir:4:25
    //    4 |         v4 = sub v1, v2; ???
    //                                 ^

    eprintln!("error: {}", error.message);
    eprintln!(
        "  at {}:{}:{}",
        error.source_name,
        error.line_number,
        error.from + 1
    );

    let left_info = format!(" {} | ", error.line_number);
    eprintln!("{}{}", left_info, error.source_line);
    eprintln!(
        "{}{}",
        " ".repeat(left_info.len() + error.from),
        "^".repeat(error.to - error.from)
    );
}

pub struct ErrorReport {
    source_name: String,
    source_line: String,
    line_number: usize,
    from: usize,
    to: usize,
    message: String,
}
