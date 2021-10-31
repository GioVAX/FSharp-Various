module Trie

type Trie = {
    Branches: Map<char,Trie>
    EndOfWord : bool
}