import requests
import re

readme_url = "https://gitlab.com/tsoding/porth/-/raw/master/README.md"
INTRINSICS_HEADER = "### Intrinsics (Built-in Words)"
SYSTEM_CALLS_NUM_OF_SUPPORTED_DIGITS = 6


def parse():
    readme_content = requests.get(readme_url).text
    intrinsic_tables = readme_content.split(INTRINSICS_HEADER)[1]
    intrinsic_words_regexp = (
        "\\|\\s*`([^|`]*)`\\s*"
        "\\|\\s*`([^|`]*)`\\s*"
        "\\|\\s*([^|]*)\\|"
        )

    full_result = re.findall(intrinsic_words_regexp, intrinsic_tables)

    intrinsic_names = list(map(lambda x: x[0], full_result))
    intrinsic_signatures = list(map(lambda x: x[1], full_result))
    intrinsic_docs = list(map(lambda x: x[2].strip(), full_result))

    intrinsics = list(zip(
        intrinsic_names,
        intrinsic_signatures,
        intrinsic_docs
    ))

    intrinsics.extend(list(not_in_table_intrinsics()))

    return to_emacs_lisp_code(intrinsics)


def not_in_table_intrinsics():
    for i in range(SYSTEM_CALLS_NUM_OF_SUPPORTED_DIGITS):
        yield (
            f"syscall{i}",
            "",
            f"perform a syscall with {i} arguments where {i} is in range"
        )

    yield (
        "here",
        "a -- a [len: int] [str: ptr]",
        'pushes a string "<file-path>:<row>:<col>"'
    )

    yield "argc", "-- [argc: int]", ""
    yield "argv", "-- [argv: ptr]", ""


def to_emacs_lisp_code(obj):
    if type(obj) == list:
        list_elements_as_elisp_code = map(to_emacs_lisp_code, obj)

        return "'(" + " ".join(list_elements_as_elisp_code) + ")"

    if type(obj) == tuple:
        tuple_statement = "({})"
        list_elements_as_elisp_code = map(to_emacs_lisp_code, obj)
        tuple_body = " ".join(list_elements_as_elisp_code)

        return tuple_statement.format(tuple_body)

    elif type(obj) == str:
        return '"' + obj + '"'

    elif type(obj) == int:
        return obj

    else:
        raise NotImplementedError


def align_elisp_list(elisp_list_str):
    return elisp_list_str.replace('" "', '"\n"').replace(") (", ")\n(")


if __name__ == '__main__':
    print(align_elisp_list(parse()))
