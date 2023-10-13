;; -*-lexical-binding: t -*-

(defun c++-ts-mode-indent-style ()
  `((c-ts-mode--for-each-tail-body-matcher prev-line c-ts-mode-indent-offset)

    ((parent-is "translation_unit") column-0 0)
    ((query "(ERROR (ERROR)) @indent") column-0 0)
    ((node-is ")") parent 1)
    ((node-is "]") parent-bol 0)
    ((node-is "else") parent-bol 0)
    ((node-is "case") parent-bol 0)
    ((node-is "preproc_arg") no-indent)
    ;; `c-ts-common-looking-at-star' has to come before
    ;; `c-ts-common-comment-2nd-line-matcher'.
    ((and (parent-is "comment") c-ts-common-looking-at-star)
     c-ts-common-comment-start-after-first-star
     -1)
    (c-ts-common-comment-2nd-line-matcher c-ts-common-comment-2nd-line-anchor 1)
    ((parent-is "comment") prev-adaptive-prefix 0)

    ;; Labels.
    ((node-is "labeled_statement") standalone-parent 0)
    ((parent-is "labeled_statement")
     c-ts-mode--standalone-grandparent
     c-ts-mode-indent-offset)

    ;; Preproc directives
    ((node-is "preproc") column-0 0)
    ((node-is "#endif") column-0 0)
    ((match "preproc_call" "compound_statement") column-0 0)

    ;; Top-level things under a preproc directive.  Note that
    ;; "preproc" matches more than one type: it matches
    ;; preproc_if, preproc_elif, etc.
    ((n-p-gp nil "preproc" "translation_unit") column-0 0)
    ;; Indent rule for an empty line after a preproc directive.
    ((and no-node (parent-is ,(rx (or "\n" "preproc"))))
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode--preproc-offset)
    ;; Statement under a preproc directive, the first statement
    ;; indents against parent, the rest statements indent to
    ;; their prev-sibling.
    ((match nil ,(rx "preproc_" (or "if" "elif")) nil 3 3)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((match nil "preproc_ifdef" nil 2 2)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((match nil "preproc_else" nil 1 1)
     c-ts-mode--standalone-parent-skip-preproc
     c-ts-mode-indent-offset)
    ((parent-is "preproc") c-ts-mode--anchor-prev-sibling 0)

    ((parent-is "function_definition") parent-bol 0)
    ((parent-is "conditional_expression") first-sibling 0)
    ((parent-is "assignment_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "concatenated_string") first-sibling 0)
    ((parent-is "comma_expression") first-sibling 0)
    ((parent-is "init_declarator") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parenthesized_expression") parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "binary_expression") parent 0)
    ((query "(for_statement initializer: (_) @indent)") parent-bol 5)
    ((query "(for_statement condition: (_) @indent)") parent-bol 5)
    ((query "(for_statement update: (_) @indent)") parent-bol 5)
    ((query "(call_expression arguments: (_) @indent)")
     parent
     c-ts-mode-indent-offset)
    ((parent-is "call_expression") parent 0)
    ;; Closing bracket.  This should be before initializer_list
    ;; (and probably others) rule because that rule (and other
    ;; similar rules) will match the closing bracket.  (Bug#61398)
    ((node-is "}") standalone-parent 0)
    ((node-is "access_specifier") parent-bol 0)

    ((parent-is "declaration_list") parent-bol 0)
    ((parent-is "initializer_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "enumerator_list") parent-bol c-ts-mode-indent-offset)
    ((parent-is "field_declaration_list") parent-bol c-ts-mode-indent-offset)

    ;; Statement in {} blocks.
    (
     (or (match nil "compound_statement" nil 1 1)
         (match null "compound_statement"))
     standalone-parent c-ts-mode-indent-offset)
    ((parent-is "compound_statement") c-ts-mode--anchor-prev-sibling 0)
    ;; Opening bracket.
    ((node-is "compound_statement") standalone-parent c-ts-mode-indent-offset)
    ;; Bug#61291.
    ((match "expression_statement" nil "body")
     standalone-parent
     c-ts-mode-indent-offset)
    ;; These rules are for cases where the body is bracketless.
    ;; Tested by the "Bracketless Simple Statement" test.
    ((parent-is "if_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "for_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "while_statement") standalone-parent c-ts-mode-indent-offset)
    ((parent-is "do_statement") standalone-parent c-ts-mode-indent-offset)

    ((parent-is "case_statement") standalone-parent c-ts-mode-indent-offset)

    ((node-is "field_initializer_list")
     parent-bol
     ,(* c-ts-mode-indent-offset 2))))

(setq c-ts-mode-indent-style 'c++-ts-mode-indent-style)

(provide 'c++-init)
