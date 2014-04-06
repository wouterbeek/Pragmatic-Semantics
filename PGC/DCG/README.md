# PGC: DCG

Generic Prolog modules with Descriptive Clause Grammars.
This includes collections of DCG rules and extensions to the PGC system
(see module [dcg_generic.pl]).

## Contents

The DCG subcollection of the PGC contains the following Prolog modules:
  * [dcg_ascii.pl] Contains DCG rules for the ASCII symbols and for
    often used DCG categories.
  * [dcg_c.pl] DCG rules for supporting expressions
    in the C programming language.
  * [dcg_cardinal.pl] DCG rules for the cardinal numbers.
  * [dcg_century.pl] DCG rules for century descriptions.
  * [dcg_content.pl] DCG rules for often-occurring content that does
    not belong to a specific module.
  * [dcg_copyright.pl] DCG rules for copyright statements.
  * [dcg_date.pl] DCG rules for dates, months, and days.
  * [dcg_dict.pl] DCG-based dictionary.
  * [dcg_generic.pl] Generic extensions for the use of DCGs
    (e.g., meta-calls for DCGs, peeking, REs, replacements).
  * [dcg_multi.pl] Multiple occurrences of the same DCG rule.
  * [dcg_ordinal.pl] DCG rules for the ordinal numbers.
  * [dcg_os.pl] DCG rules for OS-dependent symbols and sequences
    (e.g., newlines).
  * [dcg_page.pl] DCG rules for pagination.
  * [dcg_print.pl] DCG rules for prints information.
  * [dcg_unicode.pl] DCG rules for Unicode support.
  * [dcg_volume.pl] DCG rules for print volumes.
  * [dcg_wrap.pl] DCG rules for text wrapping
    (e.g., line wrapping, word wrapping).
  * [dcg_year.pl] DCG rules for years and year-delimited intervals.
  * [emoticons.pl] DCG rules for emoticons :-)

@author Anne Ogborn
@author Wouter Beek
