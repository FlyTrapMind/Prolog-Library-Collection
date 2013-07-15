:- module(
  dcg_ascii,
  [
    a//0,
    a//1,
    a_lowercase//0,
    a_lowercase//1,
    a_uppercase//0,
    a_uppercase//1,
    acknowledgement//0,
    acknowledgement//1,
    alpha_numeric//0,
    alpha_numeric//1,
    ampersand//0,
    ampersand//1,
    ampersat//0,
    apetail//0,
    apetail//1,
    ampersat//1,
    apostrophe//0,
    apostrophe//1,
    ascii//0,
    ascii//1,
    asterisk//0,
    asterisk//1,
    at_sign//0,
    at_sign//1,
    at_symbol//0,
    at_symbol//1,
    b//0,
    b//1,
    b_lowercase//0,
    b_lowercase//1,
    b_uppercase//0,
    b_uppercase//1,
    backslash//0,
    backslash//1,
    backspace//0,
    backspace//1,
    bell//0,
    bell//1,
    bracket//0,
    bracket//1,
    c//0,
    c//1,
    c_lowercase//0,
    c_lowercase//1,
    c_uppercase//0,
    c_uppercase//1,
    cancel//0,
    cancel//1,
    caret//0,
    caret//1,
    carriage_return//0,
    carriage_return//1,
    circle_bracket//0,
    circle_bracket//1,
    closing_bracket//0,
    closing_bracket//1,
    closing_curly_bracket//0,
    closing_curly_bracket//1,
    closing_round_bracket//0,
    closing_round_bracket//1,
    closing_square_bracket//0,
    closing_square_bracket//1,
    colon//0,
    colon//1,
    comma//0,
    comma//1,
    commercial_at//0,
    commercial_at//1,
    control//0,
    control//1,
    copyright//0,
    copyright//1,
    curly_bracket//0,
    curly_bracket//1,
    d//0,
    d//1,
    d_lowercase//0,
    d_lowercase//1,
    d_uppercase//0,
    d_uppercase//1,
    data_link_escape//0,
    data_link_escape//1,
    dcg_delete//0,
    dcg_delete//1,
    device_control//0,
    device_control//1,
    device_control_1//0,
    device_control_1//1,
    device_control_2//0,
    device_control_2//1,
    device_control_3//0,
    device_control_3//1,
    device_control_4//0,
    device_control_4//1,
    dollar_sign//0,
    dollar_sign//1,
    dot//0,
    dot//1,
    double_quote//0,
    double_quote//1,
    e//0,
    e//1,
    e_lowercase//0,
    e_lowercase//1,
    e_uppercase//0,
    e_uppercase//1,
    eight//0,
    eight//1,
    enquiry//0,
    enquiry//1,
    end_of_line//0,
    end_of_line//1,
    end_of_medium//0,
    end_of_medium//1,
    end_of_text//0,
    end_of_text//1,
    end_of_transmission//0,
    end_of_transmission//1,
    end_of_transmission_block//0,
    end_of_transmission_block//1,
    equals_sign//0,
    equals_sign//1,
    escape//0,
    escape//1,
    exclamation_mark//0,
    exclamation_mark//1,
    f//0,
    f//1,
    f_lowercase//0,
    f_lowercase//1,
    f_uppercase//0,
    f_uppercase//1,
    file_separator//0,
    file_separator//1,
    five//0,
    five//1,
    form_feed//0,
    form_feed//1,
    forward_slash//0,
    forward_slash//1,
    four//0,
    four//1,
    g//0,
    g//1,
    g_lowercase//0,
    g_lowercase//1,
    g_uppercase//0,
    g_uppercase//1,
    dcg_graph//0,
    dcg_graph//1,
    grave_accent//0,
    grave_accent//1,
    greater_than_sign//0,
    greater_than_sign//1,
    group_separator//0,
    group_separator//1,
    h//0,
    h//1,
    h_lowercase//0,
    h_lowercase//1,
    h_uppercase//0,
    h_uppercase//1,
    horizontal_tab//0,
    horizontal_tab//1,
    hyphen//0,
    hyphen//1,
    hyphen_minus//0,
    hyphen_minus//1,
    i//0,
    i//1,
    i_lowercase//0,
    i_lowercase//1,
    i_uppercase//0,
    i_uppercase//1,
    j//0,
    j//1,
    j_lowercase//0,
    j_lowercase//1,
    j_uppercase//0,
    j_uppercase//1,
    k//0,
    k//1,
    k_lowercase//0,
    k_lowercase//1,
    k_uppercase//0,
    k_uppercase//1,
    l//0,
    l//1,
    l_lowercase//0,
    l_lowercase//1,
    l_uppercase//0,
    l_uppercase//1,
    less_than_sign//0,
    less_than_sign//1,
    letter//0,
    letter//1,
    letter_lowercase//0,
    letter_lowercase//1,
    letter_uppercase//0,
    letter_uppercase//1,
    line_feed//0,
    line_feed//1,
    m//0,
    m//1,
    m_lowercase//0,
    m_lowercase//1,
    m_uppercase//0,
    m_uppercase//1,
    minus_sign//0,
    minus_sign//1,
    n//0,
    n//1,
    n_lowercase//0,
    n_lowercase//1,
    n_uppercase//0,
    n_uppercase//1,
    negative_acknowledgement//0,
    negative_acknowledgement//1,
    nine//0,
    nine//1,
    null//0,
    null//1,
    number_sign//0,
    number_sign//1,
    o//0,
    o//1,
    o_lowercase//0,
    o_lowercase//1,
    o_uppercase//0,
    o_uppercase//1,
    one//0,
    one//1,
    opening_bracket//0,
    opening_bracket//1,
    opening_curly_bracket//0,
    opening_curly_bracket//1,
    opening_round_bracket//0,
    opening_round_bracket//1,
    opening_square_bracket//0,
    opening_square_bracket//1,
    p//0,
    p//1,
    p_lowercase//0,
    p_lowercase//1,
    p_uppercase//0,
    p_uppercase//1,
    parenthesis//0,
    parenthesis//1,
    percent_sign//0,
    percent_sign//1,
    plus_sign//0,
    plus_sign//1,
    positive_acknowledgement//0,
    positive_acknowledgement//1,
    dcg_print//0,
    dcg_print//1,
    punctuation//0,
    punctuation//1,
    q//0,
    q//1,
    q_lowercase//0,
    q_lowercase//1,
    q_uppercase//0,
    q_uppercase//1,
    question_mark//0,
    question_mark//1,
    r//0,
    r//1,
    r_lowercase//0,
    r_lowercase//1,
    r_uppercase//0,
    r_uppercase//1,
    record_separator//0,
    record_separator//1,
    round_bracket//0,
    round_bracket//1,
    s//0,
    s//1,
    s_lowercase//0,
    s_lowercase//1,
    s_uppercase//0,
    s_uppercase//1,
    semi_colon//0,
    semi_colon//1,
    seven//0,
    seven//1,
    shift//0,
    shift//1,
    shift_in//0,
    shift_in//1,
    shift_out//0,
    shift_out//1,
    six//0,
    six//1,
    slash//0,
    slash//1,
    soft_bracket//0,
    soft_bracket//1,
    space//0,
    space//1,
    square_bracket//0,
    square_bracket//1,
    start_of_heading//0,
    start_of_heading//1,
    start_of_text//0,
    start_of_text//1,
    substitute//0,
    substitute//1,
    synchronous_idle//0,
    synchronous_idle//1,
    t//0,
    t//1,
    t_lowercase//0,
    t_lowercase//1,
    t_uppercase//0,
    t_uppercase//1,
    dcg_tab//0,
    dcg_tab//1,
    three//0,
    three//1,
    tilde//0,
    tilde//1,
    two//0,
    two//1,
    u//0,
    u//1,
    u_lowercase//0,
    u_lowercase//1,
    u_uppercase//0,
    u_uppercase//1,
    underscore//0,
    underscore//1,
    unit_separator//0,
    unit_separator//1,
    v//0,
    v//1,
    v_lowercase//0,
    v_lowercase//1,
    v_uppercase//0,
    v_uppercase//1,
    vertical_bar//0,
    vertical_bar//1,
    vertical_tab//0,
    vertical_tab//1,
    w//0,
    w//1,
    w_lowercase//0,
    w_lowercase//1,
    w_uppercase//0,
    w_uppercase//1,
    dcg_white//0,
    dcg_white//1,
    x//0,
    x//1,
    x_lowercase//0,
    x_lowercase//1,
    x_uppercase//0,
    x_uppercase//1,
    y//0,
    y//1,
    y_lowercase//0,
    y_lowercase//1,
    y_uppercase//0,
    y_uppercase//1,
    z//0,
    z//1,
    z_lowercase//0,
    z_lowercase//1,
    z_uppercase//0,
    z_uppercase//1,
    zero//0,
    zero//1
  ]
).

/** <module> DCG_ASCII

DCG rules that encode the ASCII standard.

There are several different variations of the 8-bit ASCII table.
The table below is according to ISO 8859-1, also called ISO Latin-1.
Codes 129-159 contain the Microsoft® Windows Latin-1 extended characters.

---+ Alternative names

Some DCG rules are prepended with =|dcg_|=, since they would otherwise
conflict with builtins or with predicates from other modules:
  * `delete`
  * `graph`
  * `print`
  * `tab`

@author Wouter Beek
@compat http://www.ascii-code.com/
@version 2013/01-2013/02, 2013/05-2013/07
*/

:- use_module(library(dcg/basics)).
:- use_module(dcg(dcg_cardinal)).



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

alpha_numeric --> letter.
alpha_numeric --> decimal_digit.
alpha_numeric(C) --> letter(C).
alpha_numeric(C) --> decimal_digit(_N, C).

ampersand --> [38].
ampersand(38) --> [38].

ampersat --> at_sign.
ampersat(C) --> at_sign(C).

apetail --> at_sign.
apetail(C) --> at_sign(C).

apostrophe --> [39].
apostrophe(39) --> [39].

ascii --> control.
ascii --> dcg_graph.
ascii --> dcg_white.
ascii(C) --> control(C).
ascii(C) --> dcg_graph(C).
ascii(C) --> dcg_white(C).

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

bracket --> closing_bracket.
bracket --> opening_bracket.
bracket(C) --> closing_bracket(C).
bracket(C) --> opening_bracket(C).

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

closing_bracket --> closing_curly_bracket.
closing_bracket --> closing_round_bracket.
closing_bracket --> closing_square_bracket.

closing_bracket(C) --> closing_curly_bracket(C).
closing_bracket(C) --> closing_round_bracket(C).
closing_bracket(C) --> closing_square_bracket(C).

closing_curly_bracket --> [125].
closing_curly_bracket(125) --> [125].

closing_round_bracket --> [41].
closing_round_bracket(41) --> [41].

closing_square_bracket --> [93].
closing_square_bracket(93) --> [93].

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
control --> dcg_tab.
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
control(C) --> dcg_tab(C).
control(C) --> unit_separator(C).

copyright --> [169].
copyright(169) --> [169].

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

end_of_line --> carriage_return.
end_of_line --> end_of_medium.
end_of_line --> end_of_text.
end_of_line --> end_of_transmission.
end_of_line --> end_of_transmission_block.
end_of_line --> line_feed.

end_of_line(C) --> carriage_return(C).
end_of_line(C) --> end_of_medium(C).
end_of_line(C) --> end_of_text(C).
end_of_line(C) --> end_of_transmission(C).
end_of_line(C) --> end_of_transmission_block(C).
end_of_line(C) --> line_feed(C).

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

dcg_graph --> alpha_numeric.
dcg_graph --> punctuation.
dcg_graph(C) --> alpha_numeric(C).
dcg_graph(C) --> punctuation(C).

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

letter --> letter_lowercase.
letter --> letter_uppercase.
letter(C) --> letter_lowercase(C).
letter(C) --> letter_uppercase(C).

letter_lowercase --> a_lowercase.
letter_lowercase --> b_lowercase.
letter_lowercase --> c_lowercase.
letter_lowercase --> d_lowercase.
letter_lowercase --> e_lowercase.
letter_lowercase --> f_lowercase.
letter_lowercase --> g_lowercase.
letter_lowercase --> h_lowercase.
letter_lowercase --> i_lowercase.
letter_lowercase --> j_lowercase.
letter_lowercase --> k_lowercase.
letter_lowercase --> l_lowercase.
letter_lowercase --> m_lowercase.
letter_lowercase --> n_lowercase.
letter_lowercase --> o_lowercase.
letter_lowercase --> p_lowercase.
letter_lowercase --> q_lowercase.
letter_lowercase --> r_lowercase.
letter_lowercase --> s_lowercase.
letter_lowercase --> t_lowercase.
letter_lowercase --> u_lowercase.
letter_lowercase --> v_lowercase.
letter_lowercase --> w_lowercase.
letter_lowercase --> x_lowercase.
letter_lowercase --> y_lowercase.
letter_lowercase --> z_lowercase.
letter_lowercase(C) --> a_lowercase(C).
letter_lowercase(C) --> b_lowercase(C).
letter_lowercase(C) --> c_lowercase(C).
letter_lowercase(C) --> d_lowercase(C).
letter_lowercase(C) --> e_lowercase(C).
letter_lowercase(C) --> f_lowercase(C).
letter_lowercase(C) --> g_lowercase(C).
letter_lowercase(C) --> h_lowercase(C).
letter_lowercase(C) --> i_lowercase(C).
letter_lowercase(C) --> j_lowercase(C).
letter_lowercase(C) --> k_lowercase(C).
letter_lowercase(C) --> l_lowercase(C).
letter_lowercase(C) --> m_lowercase(C).
letter_lowercase(C) --> n_lowercase(C).
letter_lowercase(C) --> o_lowercase(C).
letter_lowercase(C) --> p_lowercase(C).
letter_lowercase(C) --> q_lowercase(C).
letter_lowercase(C) --> r_lowercase(C).
letter_lowercase(C) --> s_lowercase(C).
letter_lowercase(C) --> t_lowercase(C).
letter_lowercase(C) --> u_lowercase(C).
letter_lowercase(C) --> v_lowercase(C).
letter_lowercase(C) --> w_lowercase(C).
letter_lowercase(C) --> x_lowercase(C).
letter_lowercase(C) --> y_lowercase(C).
letter_lowercase(C) --> z_lowercase(C).

letter_uppercase --> a_uppercase.
letter_uppercase --> b_uppercase.
letter_uppercase --> c_uppercase.
letter_uppercase --> d_uppercase.
letter_uppercase --> e_uppercase.
letter_uppercase --> f_uppercase.
letter_uppercase --> g_uppercase.
letter_uppercase --> h_uppercase.
letter_uppercase --> i_uppercase.
letter_uppercase --> j_uppercase.
letter_uppercase --> k_uppercase.
letter_uppercase --> l_uppercase.
letter_uppercase --> m_uppercase.
letter_uppercase --> n_uppercase.
letter_uppercase --> o_uppercase.
letter_uppercase --> p_uppercase.
letter_uppercase --> q_uppercase.
letter_uppercase --> r_uppercase.
letter_uppercase --> s_uppercase.
letter_uppercase --> t_uppercase.
letter_uppercase --> u_uppercase.
letter_uppercase --> v_uppercase.
letter_uppercase --> w_uppercase.
letter_uppercase --> x_uppercase.
letter_uppercase --> y_uppercase.
letter_uppercase --> z_uppercase.
letter_uppercase(C) --> a_uppercase(C).
letter_uppercase(C) --> b_uppercase(C).
letter_uppercase(C) --> c_uppercase(C).
letter_uppercase(C) --> d_uppercase(C).
letter_uppercase(C) --> e_uppercase(C).
letter_uppercase(C) --> f_uppercase(C).
letter_uppercase(C) --> g_uppercase(C).
letter_uppercase(C) --> h_uppercase(C).
letter_uppercase(C) --> i_uppercase(C).
letter_uppercase(C) --> j_uppercase(C).
letter_uppercase(C) --> k_uppercase(C).
letter_uppercase(C) --> l_uppercase(C).
letter_uppercase(C) --> m_uppercase(C).
letter_uppercase(C) --> n_uppercase(C).
letter_uppercase(C) --> o_uppercase(C).
letter_uppercase(C) --> p_uppercase(C).
letter_uppercase(C) --> q_uppercase(C).
letter_uppercase(C) --> r_uppercase(C).
letter_uppercase(C) --> s_uppercase(C).
letter_uppercase(C) --> t_uppercase(C).
letter_uppercase(C) --> u_uppercase(C).
letter_uppercase(C) --> v_uppercase(C).
letter_uppercase(C) --> w_uppercase(C).
letter_uppercase(C) --> x_uppercase(C).
letter_uppercase(C) --> y_uppercase(C).
letter_uppercase(C) --> z_uppercase(C).

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

one --> [49].
one(49) --> [49].

opening_bracket --> opening_curly_bracket.
opening_bracket --> opening_round_bracket.
opening_bracket --> opening_square_bracket.
opening_bracket(C) --> opening_curly_bracket(C).
opening_bracket(C) --> opening_round_bracket(C).
opening_bracket(C) --> opening_square_bracket(C).

opening_curly_bracket --> [123].
opening_curly_bracket(123) --> [123].

opening_round_bracket --> [40].
opening_round_bracket(40) --> [40].

opening_square_bracket --> [91].
opening_square_bracket(91) --> [91].

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

dcg_print --> dcg_graph.
dcg_print --> space.
dcg_print(C) --> dcg_graph(C).
dcg_print(C) --> space(C).

punctuation --> ampersand.
punctuation --> apostrophe.
punctuation --> asterisk.
punctuation --> at_sign.
punctuation --> bracket.
punctuation --> caret.
punctuation --> colon.
punctuation --> comma.
punctuation --> dollar_sign.
punctuation --> dot.
punctuation --> double_quote.
punctuation --> equals_sign.
punctuation --> exclamation_mark.
punctuation --> grave_accent.
punctuation --> greater_than_sign.
punctuation --> hyphen_minus.
punctuation --> less_than_sign.
punctuation --> number_sign.
punctuation --> percent_sign.
punctuation --> plus_sign.
punctuation --> question_mark.
punctuation --> semi_colon.
punctuation --> slash.
punctuation --> tilde.
punctuation --> underscore.
punctuation --> vertical_bar.
punctuation(C) --> ampersand(C).
punctuation(C) --> apostrophe(C).
punctuation(C) --> asterisk(C).
punctuation(C) --> at_sign(C).
punctuation(C) --> bracket(C).
punctuation(C) --> caret(C).
punctuation(C) --> colon(C).
punctuation(C) --> comma(C).
punctuation(C) --> dollar_sign(C).
punctuation(C) --> dot(C).
punctuation(C) --> double_quote(C).
punctuation(C) --> equals_sign(C).
punctuation(C) --> exclamation_mark(C).
punctuation(C) --> grave_accent(C).
punctuation(C) --> greater_than_sign(C).
punctuation(C) --> hyphen_minus(C).
punctuation(C) --> less_than_sign(C).
punctuation(C) --> number_sign(C).
punctuation(C) --> percent_sign(C).
punctuation(C) --> plus_sign(C).
punctuation(C) --> question_mark(C).
punctuation(C) --> semi_colon(C).
punctuation(C) --> slash(C).
punctuation(C) --> tilde(C).
punctuation(C) --> underscore(C).
punctuation(C) --> vertical_bar(C).

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

dcg_tab --> horizontal_tab.
dcg_tab --> vertical_tab.
dcg_tab(C) --> horizontal_tab(C).
dcg_tab(C) --> vertical_tab(C).

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

dcg_white --> end_of_line.
dcg_white --> form_feed.
dcg_white --> space.
dcg_white --> dcg_tab.
dcg_white(C) --> end_of_line(C).
dcg_white(C) --> form_feed(C).
dcg_white(C) --> space(C).
dcg_white(C) --> dcg_tab(C).

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
