.PHONY: check boot

boot: check
	.mooncakes/moonbitlang/yacc/moonyacc --print-as-mly-without-actions ./boot/parser.mly > ./boot/parser-no-actions.mly

check:
	moon check
