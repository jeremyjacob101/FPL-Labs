import string
from collections import OrderedDict
from pathlib import Path

VALID_LIMIT = 25
TOTAL_LIMIT = 1000


def get_valid_words(words_path: str) -> set:
    words = set()
    p = Path(words_path)
    if not p.exists():
        raise FileNotFoundError(words_path)
    with p.open(encoding="utf-8") as f:
        count = 0
        for line in f:
            w = line.strip().upper()
            if (
                not w
                or len(w) != 5
                or any(ch not in string.ascii_uppercase for ch in w)
            ):
                continue
            words.add(w)
            count += 1
            if count >= VALID_LIMIT:
                break
    return words


def build_letter_dict(words_path: str, valid_words: set) -> dict:
    count = 0
    letters = {ch: "" for ch in string.ascii_uppercase}
    p = Path(words_path)
    if not p.exists():
        raise FileNotFoundError(words_path)
    with p.open(encoding="utf-8") as f:
        for line in f:
            w = line.strip().upper()
            if not w:
                continue
            if w not in valid_words:
                continue
            first = w[0]
            if first in letters:
                letters[first] += w[1:]
            count += 1
            if count >= TOTAL_LIMIT:
                break
    print(f"Processed {count} words")
    return letters


def word_to_base26(word: str) -> tuple[int, int]:
    w = word.strip().upper()
    if len(w) != 5 or any(ch not in string.ascii_uppercase for ch in w):
        raise ValueError(f"Expected 5-letter A-Z word, got: {word!r}")

    first2 = 0
    for ch in w[:2]:
        first2 = first2 * 26 + (ord(ch) - ord("A"))

    last3 = 0
    for ch in w[2:5]:
        last3 = last3 * 26 + (ord(ch) - ord("A"))

    first2_u16 = first2 & 0xFFFF
    last3_u16 = last3 & 0xFFFF
    first2_s16 = first2_u16 - 0x10000 if first2_u16 >= 0x8000 else first2_u16
    last3_s16 = last3_u16 - 0x10000 if last3_u16 >= 0x8000 else last3_u16
    return first2_s16, last3_s16


def encode_words_file(words_path: str, valid_words: set | None = None) -> dict:
    results = OrderedDict()
    p = Path(words_path)
    if not p.exists():
        raise FileNotFoundError(words_path)

    with p.open(encoding="utf-8") as f:
        for line in f:
            word = line.strip().upper()
            if not word:
                continue
            if valid_words is not None and word not in valid_words:
                continue
            try:
                first2_s16, last3_s16 = word_to_base26(word)
            except ValueError:
                continue

            # print(f"{word.upper()} -> ({first2_s16}, {last3_s16})")
            results[word] = (first2_s16, last3_s16)

    return results


if __name__ == "__main__":
    # expects a word list in the same directory
    # valid_words = get_valid_words("top_words.txt")
    # d = build_letter_dict("top_words.txt", valid_words)
    # for k in sorted(d):
    #     print(f'let wordsByLetter[{ord(k)-65}] = "{d[k]}";')
    d = encode_words_file("wordle_words.txt", valid_words=None)
    index = 0
    for k in d:
        print(
            f"\t\tlet words[{index}] = {d[k][0]}; let words[{index+1}] = {d[k][1]}; // {k}"
        )
        index += 2
    print()
    print(f"Total words: {len(d)}")
