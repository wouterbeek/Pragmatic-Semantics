:- module(
  dcg_ascii,
  [
    a//0,
    a//1, % ?Code:code
    a_lowercase//0,
    a_lowercase//1, % ?Code:code
    a_uppercase//0,
    a_uppercase//1, % ?Code:code
    acknowledgement//0,
    acknowledgement//1, % ?Code:code
    ascii_alpha_numeric//0,
    ascii_alpha_numeric//1, % ?Code:code
    ampersand//0,
    ampersand//1, % ?Code:code
    ampersat//0,
    ampersat//1, % ?Code:code
    apetail//0,
    apetail//1, % ?Code:code
    apostrophe//0,
    apostrophe//1, % ?Code:code
    ascii//0,
    ascii//1, % ?Code:code
    asterisk//0,
    asterisk//1, % ?Code:code
    at_sign//0,
    at_sign//1, % ?Code:code
    at_symbol//0,
    at_symbol//1, % ?Code:code
    b//0,
    b//1, % ?Code:code
    b_lowercase//0,
    b_lowercase//1, % ?Code:code
    b_uppercase//0,
    b_uppercase//1, % ?Code:code
    backslash//0,
    backslash//1, % ?Code:code
    backspace//0,
    backspace//1, % ?Code:code
    bell//0,
    bell//1, % ?Code:code
    binary_digit//0,
    binary_digit//1, % ?Code:code
    bracket//0,
    bracket//1, % ?Code:code
    bracket//2, % ?Type:oneof([curly,round,square])
                % ?Code:code
    c//0,
    c//1, % ?Code:code
    c_lowercase//0,
    c_lowercase//1, % ?Code:code
    c_uppercase//0,
    c_uppercase//1, % ?Code:code
    cancel//0,
    cancel//1, % ?Code:code
    caret//0,
    caret//1, % ?Code:code
    carriage_return//0,
    carriage_return//1, % ?Code:code
    circle_bracket//0,
    circle_bracket//1, % ?Code:code
    closing_angular_bracket//0,
    closing_angular_bracket//1, % ?Code:code
    closing_angular_bracket//2, % ?Code:code
                                % ?Type:oneof([angular])
    closing_bracket//0,
    closing_bracket//1, % ?Code:code
    closing_bracket//2, % ?Code:code
                        % ?Type:oneof([angular,curly,round,square])
    closing_curly_bracket//0,
    closing_curly_bracket//1, % ?Code:code
    closing_curly_bracket//2, % ?Code:code
                              % ?Type:oneof([curly])
    closing_round_bracket//0,
    closing_round_bracket//1, % ?Code:code
    closing_round_bracket//2, % ?Code:code
                              % ?Type:oneof([round])
    closing_square_bracket//0,
    closing_square_bracket//1, % ?Code:code
    closing_square_bracket//2, % ?Code:code
                               % ?Type:oneof([square])
    colon//0,
    colon//1, % ?Code:code
    comma//0,
    comma//1, % ?Code:code
    commercial_at//0,
    commercial_at//1, % ?Code:code
    control//0,
    control//1, % ?Code:code
    copyright//0,
    copyright//1, % ?Code:code
    crosshatch//0,
    crosshatch//1, % ?Code:code
    curly_bracket//0,
    curly_bracket//1, % ?Code:code
    d//0,
    d//1, % ?Code:code
    d_lowercase//0,
    d_lowercase//1, % ?Code:code
    d_uppercase//0,
    d_uppercase//1, % ?Code:code
    data_link_escape//0,
    data_link_escape//1, % ?Code:code
    decimal_digit//0,
    decimal_digit//1, % ?Code:code
    dcg_delete//0,
    dcg_delete//1, % ?Code:code
    device_control//0,
    device_control//1, % ?Code:code
    device_control_1//0,
    device_control_1//1, % ?Code:code
    device_control_2//0,
    device_control_2//1, % ?Code:code
    device_control_3//0,
    device_control_3//1, % ?Code:code
    device_control_4//0,
    device_control_4//1, % ?Code:code
    dollar_sign//0,
    dollar_sign//1, % ?Code:code
    dot//0,
    dot//1, % ?Code:code
    double_quote//0,
    double_quote//1, % ?Code:code
    e//0,
    e//1, % ?Code:code
    e_lowercase//0,
    e_lowercase//1, % ?Code:code
    e_uppercase//0,
    e_uppercase//1, % ?Code:code
    eight//0,
    eight//1, % ?Code:code
    enquiry//0,
    enquiry//1, % ?Code:code
    ascii_end_of_line//0,
    ascii_end_of_line//1, % ?Code:code
    end_of_medium//0,
    end_of_medium//1, % ?Code:code
    end_of_text//0,
    end_of_text//1, % ?Code:code
    end_of_transmission//0,
    end_of_transmission//1, % ?Code:code
    end_of_transmission_block//0,
    end_of_transmission_block//1, % ?Code:code
    equals_sign//0,
    equals_sign//1, % ?Code:code
    escape//0,
    escape//1, % ?Code:code
    exclamation_mark//0,
    exclamation_mark//1, % ?Code:code
    f//0,
    f//1, % ?Code:code
    f_lowercase//0,
    f_lowercase//1, % ?Code:code
    f_uppercase//0,
    f_uppercase//1, % ?Code:code
    file_separator//0,
    file_separator//1, % ?Code:code
    five//0,
    five//1, % ?Code:code
    form_feed//0,
    form_feed//1, % ?Code:code
    forward_slash//0,
    forward_slash//1, % ?Code:code
    four//0,
    four//1, % ?Code:code
    g//0,
    g//1, % ?Code:code
    g_lowercase//0,
    g_lowercase//1, % ?Code:code
    g_uppercase//0,
    g_uppercase//1, % ?Code:code
    ascii_graphic//0,
    ascii_graphic//1, % ?Code:code
    grave_accent//0,
    grave_accent//1, % ?Code:code
    greater_than_sign//0,
    greater_than_sign//1, % ?Code:code
    group_separator//0,
    group_separator//1, % ?Code:code
    h//0,
    h//1, % ?Code:code
    h_lowercase//0,
    h_lowercase//1, % ?Code:code
    h_uppercase//0,
    h_uppercase//1, % ?Code:code
    hexadecimal_digit//0,
    hexadecimal_digit//1, % ?Code:code
    horizontal_tab//0,
    horizontal_tab//1, % ?Code:code
    hyphen//0,
    hyphen//1, % ?Code:code
    hyphen_minus//0,
    hyphen_minus//1, % ?Code:code
    i//0,
    i//1, % ?Code:code
    i_lowercase//0,
    i_lowercase//1, % ?Code:code
    i_uppercase//0,
    i_uppercase//1, % ?Code:code
    j//0,
    j//1, % ?Code:code
    j_lowercase//0,
    j_lowercase//1, % ?Code:code
    j_uppercase//0,
    j_uppercase//1, % ?Code:code
    k//0,
    k//1, % ?Code:code
    k_lowercase//0,
    k_lowercase//1, % ?Code:code
    k_uppercase//0,
    k_uppercase//1, % ?Code:code
    l//0,
    l//1, % ?Code:code
    l_lowercase//0,
    l_lowercase//1, % ?Code:code
    l_uppercase//0,
    l_uppercase//1, % ?Code:code
    less_than_sign//0,
    less_than_sign//1, % ?Code:code
    ascii_letter//0,
    ascii_letter//1, % ?Code:code
    ascii_letter_lowercase//0,
    ascii_letter_lowercase//1, % ?Code:code
    ascii_letter_uppercase//0,
    ascii_letter_uppercase//1, % ?Code:code
    line_feed//0,
    line_feed//1, % ?Code:code
    m//0,
    m//1, % ?Code:code
    m_lowercase//0,
    m_lowercase//1, % ?Code:code
    m_uppercase//0,
    m_uppercase//1, % ?Code:code
    minus_sign//0,
    minus_sign//1, % ?Code:code
    n//0,
    n//1, % ?Code:code
    n_lowercase//0,
    n_lowercase//1, % ?Code:code
    n_uppercase//0,
    n_uppercase//1, % ?Code:code
    negative_acknowledgement//0,
    negative_acknowledgement//1, % ?Code:code
    nine//0,
    nine//1, % ?Code:code
    null//0,
    null//1, % ?Code:code
    number_sign//0,
    number_sign//1, % ?Code:code
    o//0,
    o//1, % ?Code:code
    o_lowercase//0,
    o_lowercase//1, % ?Code:code
    o_uppercase//0,
    o_uppercase//1, % ?Code:code
    octal_digit//0,
    octal_digit//1, % ?Code:code
    one//0,
    one//1, % ?Code:code
    opening_angular_bracket//0,
    opening_angular_bracket//1, % ?Code:code
    opening_angular_bracket//2, % ?Code:code
                                % ?Type:oneof([angular])
    opening_bracket//0,
    opening_bracket//1, % ?Code:code
    opening_bracket//2, % ?Code:code
                        % ?Type:oneof([angular,curly,round,square])
    opening_curly_bracket//0,
    opening_curly_bracket//1, % ?Code:code
    opening_curly_bracket//2, % ?Code:code
                              % ?Type:oneof([curly])
    opening_round_bracket//0,
    opening_round_bracket//1, % ?Code:code
    opening_round_bracket//2, % ?Code:code
                              % ?Type:oneof([round])
    opening_square_bracket//0,
    opening_square_bracket//1, % ?Code:code
    opening_square_bracket//2, % ?Code:code
                               % ?Type:oneof([square])
    p//0,
    p//1, % ?Code:code
    p_lowercase//0,
    p_lowercase//1, % ?Code:code
    p_uppercase//0,
    p_uppercase//1, % ?Code:code
    parenthesis//0,
    parenthesis//1, % ?Code:code
    percent_sign//0,
    percent_sign//1, % ?Code:code
    plus_sign//0,
    plus_sign//1, % ?Code:code
    positive_acknowledgement//0,
    positive_acknowledgement//1, % ?Code:code
    ascii_print//0,
    ascii_print//1, % ?Code:code
    ascii_punctuation//0,
    ascii_punctuation//1, % ?Code:code
    q//0,
    q//1, % ?Code:code
    q_lowercase//0,
    q_lowercase//1, % ?Code:code
    q_uppercase//0,
    q_uppercase//1, % ?Code:code
    question_mark//0,
    question_mark//1, % ?Code:code
    r//0,
    r//1, % ?Code:code
    r_lowercase//0,
    r_lowercase//1, % ?Code:code
    r_uppercase//0,
    r_uppercase//1, % ?Code:code
    record_separator//0,
    record_separator//1, % ?Code:code
    round_bracket//0,
    round_bracket//1, % ?Code:code
    s//0,
    s//1, % ?Code:code
    s_lowercase//0,
    s_lowercase//1, % ?Code:code
    s_uppercase//0,
    s_uppercase//1, % ?Code:code
    semi_colon//0,
    semi_colon//1, % ?Code:code
    seven//0,
    seven//1, % ?Code:code
    shift//0,
    shift//1, % ?Code:code
    shift_in//0,
    shift_in//1, % ?Code:code
    shift_out//0,
    shift_out//1, % ?Code:code
    single_quote//0,
    single_quote//1, % ?Code:code
    six//0,
    six//1, % ?Code:code
    slash//0,
    slash//1, % ?Code:code
    soft_bracket//0,
    soft_bracket//1, % ?Code:code
    space//0,
    space//1, % ?Code:code
    square_bracket//0,
    square_bracket//1, % ?Code:code
    start_of_heading//0,
    start_of_heading//1, % ?Code:code
    start_of_text//0,
    start_of_text//1, % ?Code:code
    substitute//0,
    substitute//1, % ?Code:code
    synchronous_idle//0,
    synchronous_idle//1, % ?Code:code
    t//0,
    t//1, % ?Code:code
    t_lowercase//0,
    t_lowercase//1, % ?Code:code
    t_uppercase//0,
    t_uppercase//1, % ?Code:code
    ascii_tab//0,
    ascii_tab//1, % ?Code:code
    three//0,
    three//1, % ?Code:code
    tilde//0,
    tilde//1, % ?Code:code
    two//0,
    two//1, % ?Code:code
    u//0,
    u//1, % ?Code:code
    u_lowercase//0,
    u_lowercase//1, % ?Code:code
    u_uppercase//0,
    u_uppercase//1, % ?Code:code
    underscore//0,
    underscore//1, % ?Code:code
    unit_separator//0,
    unit_separator//1, % ?Code:code
    v//0,
    v//1, % ?Code:code
    v_lowercase//0,
    v_lowercase//1, % ?Code:code
    v_uppercase//0,
    v_uppercase//1, % ?Code:code
    vertical_bar//0,
    vertical_bar//1, % ?Code:code
    vertical_tab//0,
    vertical_tab//1, % ?Code:code
    w//0,
    w//1, % ?Code:code
    w_lowercase//0,
    w_lowercase//1, % ?Code:code
    w_uppercase//0,
    w_uppercase//1, % ?Code:code
    white//1, % ?Code:code
    ascii_white//0,
    ascii_white//1, % ?Code:code
    ascii_whites//0,
    x//0,
    x//1, % ?Code:code
    x_lowercase//0,
    x_lowercase//1, % ?Code:code
    x_uppercase//0,
    x_uppercase//1, % ?Code:code
    y//0,
    y//1, % ?Code:code
    y_lowercase//0,
    y_lowercase//1, % ?Code:code
    y_uppercase//0,
    y_uppercase//1, % ?Code:code
    z//0,
    z//1, % ?Code:code
    z_lowercase//0,
    z_lowercase//1, % ?Code:code
    z_uppercase//0,
    z_uppercase//1, % ?Code:code
    zero//0,
    zero//1
  ]
).

/** <module> DCG_ASCII

DCG rules that encode the ASCII standard.

There are several different variations of the 8-bit ASCII table.
The table below is according to ISO 8859-1, also called ISO Latin-1.
Codes 129-159 contain the Microsoft® Windows Latin-1 extended characters.

# Alternative names

Some DCG rules are prepended with =|dcg_|=, since they would otherwise
conflict with builtins or with predicates from other modules:
  * `delete`
  * `graph`
  * `print`
  * `tab`

@author Wouter Beek
@compat http://www.ascii-code.com/
@tbd Compare the implementation to the ANSI X3.4-1986 standard.
@version 2013/01-2013/02, 2013/05-2013/07, 2013/12
*/



a --> a_lowercase.
a --> a_uppercase.
a(C) --> a_lowercase(C).
a(C) --> a_uppercase(C).

a_lowercase --> [97].
a_lowercase(97) --> [97].

a_uppercase --> [65].
a_uppercase(65) --> [65].

acknowledgement --> negative_acknowledgement.
acknowledgement --> positive_acknowledgement.
acknowledgement(C) --> negative_acknowledgement(C).
acknowledgement(C) --> positive_acknowledgement(C).

ascii_alpha_numeric --> ascii_letter.
ascii_alpha_numeric --> decimal_digit.
ascii_alpha_numeric(C) --> ascii_letter(C).
ascii_alpha_numeric(C) --> decimal_digit(C).

ampersand --> [38].
ampersand(38) --> [38].

ampersat --> at_sign.
ampersat(C) --> at_sign(C).

apetail --> at_sign.
apetail(C) --> at_sign(C).

apostrophe --> [39].
apostrophe(39) --> [39].

ascii --> control.
ascii --> ascii_graphic.
ascii --> ascii_white.
ascii(C) --> control(C).
ascii(C) --> ascii_graphic(C).
ascii(C) --> ascii_white(C).

asterisk --> [42].
asterisk(42) --> [42].

at_sign --> [64].
at_sign(64) --> [64].

at_symbol --> at_sign.
at_symbol(C) --> at_sign(C).

b --> b_lowercase.
b --> b_uppercase.
b(C) --> b_lowercase(C).
b(C) --> b_uppercase(C).

b_lowercase --> [98].
b_lowercase(98) --> [98].

b_uppercase --> [66].
b_uppercase(66) --> [66].

backslash --> [92].
backslash(92) --> [92].

backspace --> [8].
backspace(8) --> [8].

bell --> [7].
bell(7) --> [7].

binary_digit --> zero.
binary_digit --> one.
binary_digit(C) --> zero(C).
binary_digit(C) --> one(C).

bracket --> closing_bracket.
bracket --> opening_bracket.
bracket(C) --> closing_bracket(C).
bracket(C) --> opening_bracket(C).
bracket(C, Type) --> closing_bracket(C, Type).
bracket(C, Type) --> opening_bracket(C, Type).

c --> c_lowercase.
c --> c_uppercase.
c(C) --> c_lowercase(C).
c(C) --> c_uppercase(C).

c_lowercase --> [99].
c_lowercase(99) --> [99].

c_uppercase --> [67].
c_uppercase(67) --> [67].

cancel --> [24].
cancel(24) --> [24].

caret --> [94].
caret(94) --> [94].

carriage_return --> [13].
carriage_return(13) --> [13].

circle_bracket --> round_bracket.
circle_bracket(C) --> round_bracket(C).

closing_angular_bracket --> less_than_sign.
closing_angular_bracket(X) --> less_than_sign(X).
closing_angular_bracket(X, angular) --> less_than_sign(X).

closing_bracket --> closing_angular_bracket.
closing_bracket --> closing_curly_bracket.
closing_bracket --> closing_round_bracket.
closing_bracket --> closing_square_bracket.

closing_bracket(C) --> closing_angular_bracket(C).
closing_bracket(C) --> closing_curly_bracket(C).
closing_bracket(C) --> closing_round_bracket(C).
closing_bracket(C) --> closing_square_bracket(C).

closing_bracket(C, Type) --> closing_angular_bracket(C, Type).
closing_bracket(C, Type) --> closing_curly_bracket(C, Type).
closing_bracket(C, Type) --> closing_round_bracket(C, Type).
closing_bracket(C, Type) --> closing_square_bracket(C, Type).

closing_curly_bracket --> [125].
closing_curly_bracket(125) --> [125].
closing_curly_bracket(125, curly) --> [125].

closing_round_bracket --> [41].
closing_round_bracket(41) --> [41].
closing_round_bracket(41, round) --> [41].

closing_square_bracket --> [93].
closing_square_bracket(93) --> [93].
closing_square_bracket(93, square) --> [93].

colon --> [58].
colon(58) --> [58].

comma --> [44].
comma(44) --> [44].

commercial_at --> at_sign.
commercial_at(C) --> at_sign(C).

control --> acknowledgement.
control --> backspace.
control --> bell.
control --> cancel.
control --> carriage_return.
control --> data_link_escape.
control --> dcg_delete.
control --> device_control.
control --> enquiry.
control --> end_of_medium.
control --> end_of_text.
control --> end_of_transmission.
control --> end_of_transmission_block.
control --> escape.
control --> file_separator.
control --> form_feed.
control --> group_separator.
control --> line_feed.
control --> null.
control --> record_separator.
control --> shift.
control --> start_of_heading.
control --> start_of_text.
control --> substitute.
control --> synchronous_idle.
control --> ascii_tab.
control --> unit_separator.
control(C) --> acknowledgement(C).
control(C) --> backspace(C).
control(C) --> bell(C).
control(C) --> cancel(C).
control(C) --> carriage_return(C).
control(C) --> data_link_escape(C).
control(C) --> dcg_delete(C).
control(C) --> device_control(C).
control(C) --> enquiry(C).
control(C) --> end_of_medium(C).
control(C) --> end_of_text(C).
control(C) --> end_of_transmission(C).
control(C) --> end_of_transmission_block(C).
control(C) --> escape(C).
control(C) --> file_separator(C).
control(C) --> form_feed(C).
control(C) --> group_separator(C).
control(C) --> line_feed(C).
control(C) --> null(C).
control(C) --> record_separator(C).
control(C) --> shift(C).
control(C) --> start_of_heading(C).
control(C) --> start_of_text(C).
control(C) --> substitute(C).
control(C) --> synchronous_idle(C).
control(C) --> ascii_tab(C).
control(C) --> unit_separator(C).

copyright --> [169].
copyright(169) --> [169].

crosshatch --> number_sign.
crosshatch(C) --> number_sign(C).

curly_bracket --> closing_curly_bracket.
curly_bracket --> opening_curly_bracket.
curly_bracket(C) --> closing_curly_bracket(C).
curly_bracket(C) --> opening_curly_bracket(C).

d --> d_lowercase.
d --> d_uppercase.
d(C) --> d_lowercase(C).
d(C) --> d_uppercase(C).

d_lowercase --> [100].
d_lowercase(100) --> [100].

d_uppercase --> [68].
d_uppercase(68) --> [68].

data_link_escape --> [16].
data_link_escape(16) --> [16].

decimal_digit --> octal_digit.
decimal_digit --> eight.
decimal_digit --> nine.
decimal_digit(C) --> octal_digit(C).
decimal_digit(C) --> eight(C).
decimal_digit(C) --> nine(C).

dcg_delete --> [127].
dcg_delete(127) --> [127].

device_control --> device_control_1.
device_control --> device_control_2.
device_control --> device_control_3.
device_control --> device_control_4.

device_control(C) --> device_control_1(C).
device_control(C) --> device_control_2(C).
device_control(C) --> device_control_3(C).
device_control(C) --> device_control_4(C).

device_control_1 --> [17].
device_control_1(17) --> [17].

device_control_2 --> [18].
device_control_2(18) --> [18].

device_control_3 --> [19].
device_control_3(19) --> [19].

device_control_4 --> [20].
device_control_4(20) --> [20].

dollar_sign --> [36].
dollar_sign(36) --> [36].

dot --> [46].
dot(46) --> [46].

double_quote --> [34].
double_quote(34) --> [34].

e --> e_lowercase.
e --> e_uppercase.
e(C) --> e_lowercase(C).
e(C) --> e_uppercase(C).

e_lowercase --> [101].
e_lowercase(101) --> [101].

e_uppercase --> [69].
e_uppercase(69) --> [69].

eight --> [56].
eight(56) --> [56].

enquiry --> [5].
enquiry(5) --> [5].

ascii_end_of_line --> carriage_return.
ascii_end_of_line --> end_of_medium.
ascii_end_of_line --> end_of_text.
ascii_end_of_line --> end_of_transmission.
ascii_end_of_line --> end_of_transmission_block.
ascii_end_of_line --> line_feed.
ascii_end_of_line(C) --> carriage_return(C).
ascii_end_of_line(C) --> end_of_medium(C).
ascii_end_of_line(C) --> end_of_text(C).
ascii_end_of_line(C) --> end_of_transmission(C).
ascii_end_of_line(C) --> end_of_transmission_block(C).
ascii_end_of_line(C) --> line_feed(C).

end_of_medium --> [25].
end_of_medium(25) --> [25].

end_of_text --> [3].
end_of_text(3) --> [3].

end_of_transmission --> [4].
end_of_transmission(4) --> [4].

end_of_transmission_block --> [23].
end_of_transmission_block(23) --> [23].

equals_sign --> [61].
equals_sign(61) --> [61].

escape --> [27].
escape(27) --> [27].

exclamation_mark --> [33].
exclamation_mark(33) --> [33].

f --> f_lowercase.
f --> f_uppercase.
f(C) --> f_lowercase(C).
f(C) --> f_uppercase(C).

f_lowercase --> [102].
f_lowercase(102) --> [102].

f_uppercase --> [70].
f_uppercase(70) --> [70].

file_separator --> [28].
file_separator(28) --> [28].

five --> [53].
five(53) --> [53].

form_feed --> [12].
form_feed(12) --> [12].

forward_slash --> [47].
forward_slash(47) --> [47].

four --> [52].
four(52) --> [52].

g --> g_lowercase.
g --> g_uppercase.
g(C) --> g_lowercase(C).
g(C) --> g_uppercase(C).

g_lowercase --> [103].
g_lowercase(103) --> [103].

g_uppercase --> [71].
g_uppercase(71) --> [71].

ascii_graphic --> ascii_alpha_numeric.
ascii_graphic --> ascii_punctuation.
ascii_graphic(C) --> ascii_alpha_numeric(C).
ascii_graphic(C) --> ascii_punctuation(C).

grave_accent --> [96].
grave_accent(96) --> [96].

greater_than_sign --> [62].
greater_than_sign(62) --> [62].

group_separator --> [29].
group_separator(29) --> [29].

h --> h_lowercase.
h --> h_uppercase.
h(C) --> h_lowercase(C).
h(C) --> h_uppercase(C).

h_lowercase --> [104].
h_lowercase(104) --> [104].

h_uppercase --> [72].
h_uppercase(72) --> [72].

hexadecimal_digit --> decimal_digit.
hexadecimal_digit --> a.
hexadecimal_digit --> b.
hexadecimal_digit --> c.
hexadecimal_digit --> d.
hexadecimal_digit --> e.
hexadecimal_digit --> f.
hexadecimal_digit(C) --> decimal_digit(C).
hexadecimal_digit(C) --> a(C).
hexadecimal_digit(C) --> b(C).
hexadecimal_digit(C) --> c(C).
hexadecimal_digit(C) --> d(C).
hexadecimal_digit(C) --> e(C).
hexadecimal_digit(C) --> f(C).

horizontal_tab --> [9].
horizontal_tab(9) --> [9].

hyphen --> hyphen_minus.
hyphen(C) --> hyphen_minus(C).

hyphen_minus --> [45].
hyphen_minus(45) --> [45].

i --> i_lowercase.
i --> i_uppercase.
i(C) --> i_lowercase(C).
i(C) --> i_uppercase(C).

i_lowercase --> [105].
i_lowercase(105) --> [105].

i_uppercase --> [73].
i_uppercase(73) --> [73].

j --> j_lowercase.
j --> j_uppercase.
j(C) --> j_lowercase(C).
j(C) --> j_uppercase(C).

j_lowercase --> [106].
j_lowercase(106) --> [106].

j_uppercase --> [74].
j_uppercase(74) --> [74].

k --> k_lowercase.
k --> k_uppercase.
k(C) --> k_lowercase(C).
k(C) --> k_uppercase(C).

k_lowercase --> [107].
k_lowercase(107) --> [107].

k_uppercase --> [75].
k_uppercase(75) --> [75].

l --> l_lowercase.
l --> l_uppercase.
l(C) --> l_lowercase(C).
l(C) --> l_uppercase(C).

l_lowercase --> [108].
l_lowercase(108) --> [108].

l_uppercase --> [76].
l_uppercase(76) --> [76].

less_than_sign --> [60].
less_than_sign(60) --> [60].

ascii_letter --> ascii_letter_lowercase.
ascii_letter --> ascii_letter_uppercase.
ascii_letter(C) --> ascii_letter_lowercase(C).
ascii_letter(C) --> ascii_letter_uppercase(C).

ascii_letter_lowercase --> a_lowercase.
ascii_letter_lowercase --> b_lowercase.
ascii_letter_lowercase --> c_lowercase.
ascii_letter_lowercase --> d_lowercase.
ascii_letter_lowercase --> e_lowercase.
ascii_letter_lowercase --> f_lowercase.
ascii_letter_lowercase --> g_lowercase.
ascii_letter_lowercase --> h_lowercase.
ascii_letter_lowercase --> i_lowercase.
ascii_letter_lowercase --> j_lowercase.
ascii_letter_lowercase --> k_lowercase.
ascii_letter_lowercase --> l_lowercase.
ascii_letter_lowercase --> m_lowercase.
ascii_letter_lowercase --> n_lowercase.
ascii_letter_lowercase --> o_lowercase.
ascii_letter_lowercase --> p_lowercase.
ascii_letter_lowercase --> q_lowercase.
ascii_letter_lowercase --> r_lowercase.
ascii_letter_lowercase --> s_lowercase.
ascii_letter_lowercase --> t_lowercase.
ascii_letter_lowercase --> u_lowercase.
ascii_letter_lowercase --> v_lowercase.
ascii_letter_lowercase --> w_lowercase.
ascii_letter_lowercase --> x_lowercase.
ascii_letter_lowercase --> y_lowercase.
ascii_letter_lowercase --> z_lowercase.
ascii_letter_lowercase(C) --> a_lowercase(C).
ascii_letter_lowercase(C) --> b_lowercase(C).
ascii_letter_lowercase(C) --> c_lowercase(C).
ascii_letter_lowercase(C) --> d_lowercase(C).
ascii_letter_lowercase(C) --> e_lowercase(C).
ascii_letter_lowercase(C) --> f_lowercase(C).
ascii_letter_lowercase(C) --> g_lowercase(C).
ascii_letter_lowercase(C) --> h_lowercase(C).
ascii_letter_lowercase(C) --> i_lowercase(C).
ascii_letter_lowercase(C) --> j_lowercase(C).
ascii_letter_lowercase(C) --> k_lowercase(C).
ascii_letter_lowercase(C) --> l_lowercase(C).
ascii_letter_lowercase(C) --> m_lowercase(C).
ascii_letter_lowercase(C) --> n_lowercase(C).
ascii_letter_lowercase(C) --> o_lowercase(C).
ascii_letter_lowercase(C) --> p_lowercase(C).
ascii_letter_lowercase(C) --> q_lowercase(C).
ascii_letter_lowercase(C) --> r_lowercase(C).
ascii_letter_lowercase(C) --> s_lowercase(C).
ascii_letter_lowercase(C) --> t_lowercase(C).
ascii_letter_lowercase(C) --> u_lowercase(C).
ascii_letter_lowercase(C) --> v_lowercase(C).
ascii_letter_lowercase(C) --> w_lowercase(C).
ascii_letter_lowercase(C) --> x_lowercase(C).
ascii_letter_lowercase(C) --> y_lowercase(C).
ascii_letter_lowercase(C) --> z_lowercase(C).

ascii_letter_uppercase --> a_uppercase.
ascii_letter_uppercase --> b_uppercase.
ascii_letter_uppercase --> c_uppercase.
ascii_letter_uppercase --> d_uppercase.
ascii_letter_uppercase --> e_uppercase.
ascii_letter_uppercase --> f_uppercase.
ascii_letter_uppercase --> g_uppercase.
ascii_letter_uppercase --> h_uppercase.
ascii_letter_uppercase --> i_uppercase.
ascii_letter_uppercase --> j_uppercase.
ascii_letter_uppercase --> k_uppercase.
ascii_letter_uppercase --> l_uppercase.
ascii_letter_uppercase --> m_uppercase.
ascii_letter_uppercase --> n_uppercase.
ascii_letter_uppercase --> o_uppercase.
ascii_letter_uppercase --> p_uppercase.
ascii_letter_uppercase --> q_uppercase.
ascii_letter_uppercase --> r_uppercase.
ascii_letter_uppercase --> s_uppercase.
ascii_letter_uppercase --> t_uppercase.
ascii_letter_uppercase --> u_uppercase.
ascii_letter_uppercase --> v_uppercase.
ascii_letter_uppercase --> w_uppercase.
ascii_letter_uppercase --> x_uppercase.
ascii_letter_uppercase --> y_uppercase.
ascii_letter_uppercase --> z_uppercase.
ascii_letter_uppercase(C) --> a_uppercase(C).
ascii_letter_uppercase(C) --> b_uppercase(C).
ascii_letter_uppercase(C) --> c_uppercase(C).
ascii_letter_uppercase(C) --> d_uppercase(C).
ascii_letter_uppercase(C) --> e_uppercase(C).
ascii_letter_uppercase(C) --> f_uppercase(C).
ascii_letter_uppercase(C) --> g_uppercase(C).
ascii_letter_uppercase(C) --> h_uppercase(C).
ascii_letter_uppercase(C) --> i_uppercase(C).
ascii_letter_uppercase(C) --> j_uppercase(C).
ascii_letter_uppercase(C) --> k_uppercase(C).
ascii_letter_uppercase(C) --> l_uppercase(C).
ascii_letter_uppercase(C) --> m_uppercase(C).
ascii_letter_uppercase(C) --> n_uppercase(C).
ascii_letter_uppercase(C) --> o_uppercase(C).
ascii_letter_uppercase(C) --> p_uppercase(C).
ascii_letter_uppercase(C) --> q_uppercase(C).
ascii_letter_uppercase(C) --> r_uppercase(C).
ascii_letter_uppercase(C) --> s_uppercase(C).
ascii_letter_uppercase(C) --> t_uppercase(C).
ascii_letter_uppercase(C) --> u_uppercase(C).
ascii_letter_uppercase(C) --> v_uppercase(C).
ascii_letter_uppercase(C) --> w_uppercase(C).
ascii_letter_uppercase(C) --> x_uppercase(C).
ascii_letter_uppercase(C) --> y_uppercase(C).
ascii_letter_uppercase(C) --> z_uppercase(C).

line_feed --> [10].
line_feed(10) --> [10].

m --> m_lowercase.
m --> m_uppercase.
m(C) --> m_lowercase(C).
m(C) --> m_uppercase(C).

m_lowercase --> [109].
m_lowercase(109) --> [109].

m_uppercase --> [77].
m_uppercase(77) --> [77].

minus_sign --> hyphen_minus.
minus_sign(C) --> hyphen_minus(C).

n --> n_lowercase.
n --> n_uppercase.
n(C) --> n_lowercase(C).
n(C) --> n_uppercase(C).

n_lowercase --> [110].
n_lowercase(110) --> [110].

n_uppercase --> [78].
n_uppercase(78) --> [78].

negative_acknowledgement --> [21].
negative_acknowledgement(21) --> [21].

nine --> [57].
nine(57) --> [57].

null --> [0].
null(0) --> [0].

number_sign --> [35].
number_sign(35) --> [35].

o --> o_lowercase.
o --> o_uppercase.
o(C) --> o_lowercase(C).
o(C) --> o_uppercase(C).

o_lowercase --> [111].
o_lowercase(111) --> [111].

o_uppercase --> [79].
o_uppercase(79) --> [79].

octal_digit --> binary_digit.
octal_digit --> two.
octal_digit --> three.
octal_digit --> four.
octal_digit --> five.
octal_digit --> six.
octal_digit --> seven.
octal_digit(C) --> binary_digit(C).
octal_digit(C) --> two(C).
octal_digit(C) --> three(C).
octal_digit(C) --> four(C).
octal_digit(C) --> five(C).
octal_digit(C) --> six(C).
octal_digit(C) --> seven(C).

one --> [49].
one(49) --> [49].

opening_angular_bracket --> greater_than_sign.
opening_angular_bracket(C) --> greater_than_sign(C).
opening_angular_bracket(C, angular) --> greater_than_sign(C).

opening_bracket --> opening_angular_bracket.
opening_bracket --> opening_curly_bracket.
opening_bracket --> opening_round_bracket.
opening_bracket --> opening_square_bracket.
opening_bracket(C) --> opening_angular_bracket(C).
opening_bracket(C) --> opening_curly_bracket(C).
opening_bracket(C) --> opening_round_bracket(C).
opening_bracket(C) --> opening_square_bracket(C).
opening_bracket(C, Type) --> opening_angular_bracket(C, Type).
opening_bracket(C, Type) --> opening_curly_bracket(C, Type).
opening_bracket(C, Type) --> opening_round_bracket(C, Type).
opening_bracket(C, Type) --> opening_square_bracket(C, Type).

opening_curly_bracket --> [123].
opening_curly_bracket(123) --> [123].
opening_curly_bracket(123, curly) --> [123].

opening_round_bracket --> [40].
opening_round_bracket(40) --> [40].
opening_round_bracket(40, round) --> [40].

opening_square_bracket --> [91].
opening_square_bracket(91) --> [91].
opening_square_bracket(91, square) --> [91].

p --> p_lowercase.
p --> p_uppercase.
p(C) --> p_lowercase(C).
p(C) --> p_uppercase(C).

p_lowercase --> [112].
p_lowercase(112) --> [112].

p_uppercase --> [80].
p_uppercase(80) --> [80].

parenthesis --> round_bracket.
parenthesis(C) --> round_bracket(C).

percent_sign --> [37].
percent_sign(37) --> [37].

plus_sign --> [43].
plus_sign(43) --> [43].

positive_acknowledgement --> [6].
positive_acknowledgement(6) --> [6].

ascii_print --> ascii_graphic.
ascii_print --> space.
ascii_print(C) --> ascii_graphic(C).
ascii_print(C) --> space(C).

ascii_punctuation --> ampersand.
ascii_punctuation --> apostrophe.
ascii_punctuation --> asterisk.
ascii_punctuation --> at_sign.
ascii_punctuation --> bracket.
ascii_punctuation --> caret.
ascii_punctuation --> colon.
ascii_punctuation --> comma.
ascii_punctuation --> dollar_sign.
ascii_punctuation --> dot.
ascii_punctuation --> double_quote.
ascii_punctuation --> equals_sign.
ascii_punctuation --> exclamation_mark.
ascii_punctuation --> grave_accent.
ascii_punctuation --> greater_than_sign.
ascii_punctuation --> hyphen_minus.
ascii_punctuation --> less_than_sign.
ascii_punctuation --> number_sign.
ascii_punctuation --> percent_sign.
ascii_punctuation --> plus_sign.
ascii_punctuation --> question_mark.
ascii_punctuation --> semi_colon.
ascii_punctuation --> slash.
ascii_punctuation --> tilde.
ascii_punctuation --> underscore.
ascii_punctuation --> vertical_bar.
ascii_punctuation(C) --> ampersand(C).
ascii_punctuation(C) --> apostrophe(C).
ascii_punctuation(C) --> asterisk(C).
ascii_punctuation(C) --> at_sign(C).
ascii_punctuation(C) --> bracket(C).
ascii_punctuation(C) --> caret(C).
ascii_punctuation(C) --> colon(C).
ascii_punctuation(C) --> comma(C).
ascii_punctuation(C) --> dollar_sign(C).
ascii_punctuation(C) --> dot(C).
ascii_punctuation(C) --> double_quote(C).
ascii_punctuation(C) --> equals_sign(C).
ascii_punctuation(C) --> exclamation_mark(C).
ascii_punctuation(C) --> grave_accent(C).
ascii_punctuation(C) --> greater_than_sign(C).
ascii_punctuation(C) --> hyphen_minus(C).
ascii_punctuation(C) --> less_than_sign(C).
ascii_punctuation(C) --> number_sign(C).
ascii_punctuation(C) --> percent_sign(C).
ascii_punctuation(C) --> plus_sign(C).
ascii_punctuation(C) --> question_mark(C).
ascii_punctuation(C) --> semi_colon(C).
ascii_punctuation(C) --> slash(C).
ascii_punctuation(C) --> tilde(C).
ascii_punctuation(C) --> underscore(C).
ascii_punctuation(C) --> vertical_bar(C).

q --> q_lowercase.
q --> q_uppercase.
q(C) --> q_lowercase(C).
q(C) --> q_uppercase(C).

q_lowercase --> [113].
q_lowercase(113) --> [113].

q_uppercase --> [81].
q_uppercase(81) --> [81].

question_mark --> [63].
question_mark(63) --> [63].

r --> r_lowercase.
r --> r_uppercase.
r(C) --> r_lowercase(C).
r(C) --> r_uppercase(C).

r_lowercase --> [114].
r_lowercase(114) --> [114].

r_uppercase --> [82].
r_uppercase(82) --> [82].

record_separator --> [30].
record_separator(30) --> [30].

round_bracket --> closing_round_bracket.
round_bracket --> opening_round_bracket.
round_bracket(C) --> closing_round_bracket(C).
round_bracket(C) --> opening_round_bracket(C).

s --> s_lowercase.
s --> s_uppercase.
s(C) --> s_lowercase(C).
s(C) --> s_uppercase(C).

s_lowercase --> [115].
s_lowercase(115) --> [115].

s_uppercase --> [83].
s_uppercase(83) --> [83].

semi_colon --> [59].
semi_colon(59) --> [59].

seven --> [55].
seven(55) --> [55].

shift --> shift_in.
shift --> shift_out.
shift(C) --> shift_in(C).
shift(C) --> shift_out(C).

shift_in --> [15].
shift_in(15) --> [15].

shift_out --> [14].
shift_out(14) --> [14].

single_quote --> apostrophe.
single_quote(C) --> apostrophe(C).

six --> [54].
six(54) --> [54].

slash --> backslash.
slash --> forward_slash.
slash(C) --> backslash(C).
slash(C) --> forward_slash(C).

soft_bracket --> round_bracket.
soft_bracket(C) --> round_bracket(C).

space --> [32].
space(32) --> [32].

square_bracket --> closing_square_bracket.
square_bracket --> opening_square_bracket.
square_bracket(C) --> closing_square_bracket(C).
square_bracket(C) --> opening_square_bracket(C).

start_of_heading --> [1].
start_of_heading(1) --> [1].

start_of_text --> [2].
start_of_text(2) --> [2].

substitute --> [26].
substitute(26) --> [26].

synchronous_idle --> [22].
synchronous_idle(22) --> [22].

t --> t_lowercase.
t --> t_uppercase.
t(C) --> t_lowercase(C).
t(C) --> t_uppercase(C).

t_lowercase --> [116].
t_lowercase(116) --> [116].

t_uppercase --> [84].
t_uppercase(84) --> [84].

ascii_tab --> horizontal_tab.
ascii_tab --> vertical_tab.
ascii_tab(C) --> horizontal_tab(C).
ascii_tab(C) --> vertical_tab(C).

three --> [51].
three(51) --> [51].

tilde --> [126].
tilde(126) --> [126].

two --> [50].
two(50) --> [50].

u --> u_lowercase.
u --> u_uppercase.
u(C) --> u_lowercase(C).
u(C) --> u_uppercase(C).

u_lowercase --> [117].
u_lowercase(117) --> [117].

u_uppercase --> [85].
u_uppercase(85) --> [85].

underscore --> [95].
underscore(95) --> [95].

unit_separator --> [31].
unit_separator(31) --> [31].

v --> v_lowercase.
v --> v_uppercase.
v(C) --> v_lowercase(C).
v(C) --> v_uppercase(C).

v_lowercase --> [118].
v_lowercase(118) --> [118].

v_uppercase --> [86].
v_uppercase(86) --> [86].

vertical_bar --> [124].
vertical_bar(124) --> [124].

vertical_tab --> [11].
vertical_tab(11) --> [11].

w --> w_lowercase.
w --> w_uppercase.
w(C) --> w_lowercase(C).
w(C) --> w_uppercase(C).

w_lowercase --> [119].
w_lowercase(119) --> [119].

w_uppercase --> [87].
w_uppercase(87) --> [87].

white(C) -->
  [C],
  {code_type(C, white)}.

ascii_white --> ascii_end_of_line.
ascii_white --> form_feed.
ascii_white --> space.
ascii_white --> ascii_tab.
ascii_white(C) --> ascii_end_of_line(C).
ascii_white(C) --> form_feed(C).
ascii_white(C) --> space(C).
ascii_white(C) --> ascii_tab(C).

ascii_whites -->
  ascii_white,
  ascii_whites.
ascii_whites --> [].

x --> x_lowercase.
x --> x_uppercase.
x(C) --> x_lowercase(C).
x(C) --> x_uppercase(C).

x_lowercase --> [120].
x_lowercase(120) --> [120].

x_uppercase --> [88].
x_uppercase(88) --> [88].

y --> y_lowercase.
y --> y_uppercase.
y(C) --> y_lowercase(C).
y(C) --> y_uppercase(C).

y_lowercase --> [121].
y_lowercase(121) --> [121].

y_uppercase --> [89].
y_uppercase(89) --> [89].

z --> z_lowercase.
z --> z_uppercase.
z(C) --> z_lowercase(C).
z(C) --> z_uppercase(C).

z_lowercase --> [122].
z_lowercase(122) --> [122].

z_uppercase --> [90].
z_uppercase(90) --> [90].

zero --> [48].
zero(48) --> [48].

