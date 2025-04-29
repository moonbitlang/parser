.PHONY: check boot

boot: check
	.mooncakes/moonbitlang/yacc/moonyacc --print-as-mly-without-actions ./boot/parser.mly > ./boot/parser-no-actions.mly
	.mooncakes/moonbitlang/yacc/moonyacc --print-as-mly-without-actions ./boot/mbti_parser.mly > ./boot/mbti_parser-no-actions.mly

check:
	moon check
