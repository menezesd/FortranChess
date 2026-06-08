#!/bin/sh
set -eu

cd "$(dirname "$0")"

output=$(
    {
        printf 'uci\n'
        printf 'isready\n'
        printf 'setoption name Clear Hash\n'
        printf 'ucinewgame\n'
        printf 'position startpos\n'
        printf 'go depth 2\n'
        sleep 1
        printf 'position fen 7k/8/5N2/8/8/8/8/6RK w - - 0 1\n'
        printf 'go depth 2\n'
        sleep 1
        printf 'quit\n'
    } | ./chess uci
)

printf '%s\n' "$output" | grep -q '^id name FortranChess$'
printf '%s\n' "$output" | grep -q '^option name Clear Hash type button$'
printf '%s\n' "$output" | grep -q '^uciok$'
printf '%s\n' "$output" | grep -q '^readyok$'
printf '%s\n' "$output" | grep -q '^bestmove d2d4$'
printf '%s\n' "$output" | grep -q '^bestmove g1g8$'

printf '%s\n' 'uci_regression: PASS'
