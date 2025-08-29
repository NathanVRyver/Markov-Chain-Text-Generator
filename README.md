# Markov Chain Text Generator

A simple bigram-based text generator that learns from a corpus and produces new text with similar statistical properties.

## How It Works

1. **Tokenization**: Splits text into words and punctuation, converting to lowercase
2. **Chain Building**: Creates a frequency map where each word points to a list of words that follow it
3. **Generation**: Starting from `<START>`, randomly samples next words based on observed frequencies
4. **Detokenization**: Reconstructs readable text with proper punctuation and capitalization

## Build & Run

```bash
# Build the project
cabal build

# Run with a text file
cabal run markov -- input.txt [max_length]

# Examples
cabal run markov -- sample.txt 50      # Generate up to 50 tokens
cabal run markov -- corpus.txt         # Default 50 tokens
```

## Implementation Details

- **Chain Order**: 1 (bigrams) - each word predicts the next
- **Data Structure**: `Map Text [Text]` - duplicates encode frequency
- **Special Tokens**: `<START>` for sentence beginnings, `<END>` for endings
- **Edge Cases**: Unknown words fallback to `<END>`, empty successor lists handled gracefully

## Example

Input corpus:
```
The cat sat on the mat. The dog ran in the park. 
The cat likes fish. The dog likes bones.
```

Possible output:
```
The cat likes fish. The dog ran in the park.
```

## Files

- `src/Main.hs` - Complete implementation in ~130 lines
- `markov-chain-text-generator.cabal` - Build configuration
- `sample.txt` - Example corpus for testing

## Complexity

- **Build**: O(n) where n = number of tokens
- **Memory**: O(v + n) where v = vocabulary size
- **Generate**: O(k) for k output tokens