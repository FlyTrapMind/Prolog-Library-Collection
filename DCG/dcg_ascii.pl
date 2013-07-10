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
    apostrophe//0,
    apostrophe//1,
    ascii//0,
    ascii//1,
    asterisk//0,
    asterisk//1,
    at_sign//0,
    at_sign//1,
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
    line_feed//0,
    line_feed//1,
    less_than_sign//0,
    less_than_sign//1,
    letter//0,
    letter//1,
    letter_lowercase//0,
    letter_lowercase//1,
    letter_uppercase//0,
    letter_uppercase//1,
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
conflict with builtins or with predicates from other modules.

  * delete
  * graph
  * print
  * tab

@author Wouter Beek
@compat http://www.ascii-code.com/
@version 2013/01-2013/02, 2013/05-2013/07
*/

:- use_module(library(dcg/basics)).
:- use_module(dcg(dcg_cardinal)).



a --> a_lowercase.
a --> a_uppercase.
a(X) --> a_lowercase(X).
a(X) --> a_uppercase(X).

a_lowercase --> [97].
a_lowercase(97) --> [97].

a_uppercase --> [65].
a_uppercase(65) --> [65].

acknowledgement --> negative_acknowledgement.
acknowledgement --> positive_acknowledgement.
acknowledgement(X) --> negative_acknowledgement(X).
acknowledgement(X) --> positive_acknowledgement(X).

alpha_numeric --> letter.
alpha_numeric --> decimal_digit.
alpha_numeric(X) --> letter(X).
alpha_numeric(X) --> decimal_digit(X).

ampersand --> [38].
ampersand(38) --> [38].

apostrophe --> [39].
apostrophe(39) --> [39].

ascii --> control.
ascii --> dcg_graph.
ascii --> dcg_white.
ascii(X) --> control(X).
ascii(X) --> dcg_graph(X).
ascii(X) --> dcg_white(X).

asterisk --> [42].
asterisk(42) --> [42].

at_sign --> [65].
at_sign(65) --> [65].

b --> b_lowercase.
b --> b_uppercase.
b(X) --> b_lowercase(X).
b(X) --> b_uppercase(X).

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
bracket(X) --> closing_bracket(X).
bracket(X) --> opening_bracket(X).

c --> c_lowercase.
c --> c_uppercase.
c(X) --> c_lowercase(X).
c(X) --> c_uppercase(X).

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

closing_bracket --> closing_curly_bracket.
closing_bracket --> closing_round_bracket.
closing_bracket --> closing_square_bracket.

closing_bracket(X) --> closing_curly_bracket(X).
closing_bracket(X) --> closing_round_bracket(X).
closing_bracket(X) --> closing_square_bracket(X).

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

control(X) --> acknowledgement(X).
control(X) --> backspace(X).
control(X) --> bell(X).
control(X) --> cancel(X).
control(X) --> carriage_return(X).
control(X) --> data_link_escape(X).
control(X) --> dcg_delete(X).
control(X) --> device_control(X).
control(X) --> enquiry(X).
control(X) --> end_of_medium(X).
control(X) --> end_of_text(X).
control(X) --> end_of_transmission(X).
control(X) --> end_of_transmission_block(X).
control(X) --> escape(X).
control(X) --> file_separator(X).
control(X) --> form_feed(X).
control(X) --> group_separator(X).
control(X) --> line_feed(X).
control(X) --> null(X).
control(X) --> record_separator(X).
control(X) --> shift(X).
control(X) --> start_of_heading(X).
control(X) --> start_of_text(X).
control(X) --> substitute(X).
control(X) --> synchronous_idle(X).
control(X) --> dcg_tab(X).
control(X) --> unit_separator(X).

copyright --> [169].
copyright(169) --> [169].

curly_bracket --> closing_curly_bracket.
curly_bracket --> opening_curly_bracket.
curly_bracket(X) --> closing_curly_bracket(X).
curly_bracket(X) --> opening_curly_bracket(X).

d --> d_lowercase.
d --> d_uppercase.
d(X) --> d_lowercase(X).
d(X) --> d_uppercase(X).

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

device_control(X) --> device_control_1(X).
device_control(X) --> device_control_2(X).
device_control(X) --> device_control_3(X).
device_control(X) --> device_control_4(X).

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
e(X) --> e_lowercase(X).
e(X) --> e_uppercase(X).

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

end_of_line(X) --> carriage_return(X).
end_of_line(X) --> end_of_medium(X).
end_of_line(X) --> end_of_text(X).
end_of_line(X) --> end_of_transmission(X).
end_of_line(X) --> end_of_transmission_block(X).
end_of_line(X) --> line_feed(X).

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
f(X) --> f_lowercase(X).
f(X) --> f_uppercase(X).

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
g(X) --> g_lowercase(X).
g(X) --> g_uppercase(X).

g_lowercase --> [103].
g_lowercase(103) --> [103].

g_uppercase --> [71].
g_uppercase(71) --> [71].

dcg_graph --> alpha_numeric.
dcg_graph --> punctuation.
dcg_graph(X) --> alpha_numeric(X).
dcg_graph(X) --> punctuation(X).

grave_accent --> [96].
grave_accent(96) --> [96].

greater_than_sign --> [62].
greater_than_sign(62) --> [62].

group_separator --> [29].
group_separator(29) --> [29].

h --> h_lowercase.
h --> h_uppercase.
h(X) --> h_lowercase(X).
h(X) --> h_uppercase(X).

h_lowercase --> [104].
h_lowercase(104) --> [104].

h_uppercase --> [72].
h_uppercase(72) --> [72].

horizontal_tab --> [9].
horizontal_tab(9) --> [9].

hyphen --> hyphen_minus.
hyphen(X) --> hyphen_minus(X).

hyphen_minus --> [45].
hyphen_minus(45) --> [45].

i --> i_lowercase.
i --> i_uppercase.
i(X) --> i_lowercase(X).
i(X) --> i_uppercase(X).

i_lowercase --> [105].
i_lowercase(105) --> [105].

i_uppercase --> [73].
i_uppercase(73) --> [73].

j --> j_lowercase.
j --> j_uppercase.

j(X) --> j_lowercase(X).
j(X) --> j_uppercase(X).

j_lowercase --> [106].
j_lowercase(106) --> [106].

j_uppercase --> [74].
j_uppercase(74) --> [74].

k --> k_lowercase.
k --> k_uppercase.
k(X) --> k_lowercase(X).
k(X) --> k_uppercase(X).

k_lowercase --> [107].
k_lowercase(107) --> [107].

k_uppercase --> [75].
k_uppercase(75) --> [75].

l --> l_lowercase.
l --> l_uppercase.
l(X) --> l_lowercase(X).
l(X) --> l_uppercase(X).

l_lowercase --> [108].
l_lowercase(108) --> [108].

l_uppercase --> [76].
l_uppercase(76) --> [76].

line_feed --> [10].
line_feed(10) --> [10].

less_than_sign --> [60].
less_than_sign(60) --> [60].

letter --> letter_lowercase.
letter --> letter_uppercase.
letter(X) --> letter_lowercase(X).
letter(X) --> letter_uppercase(X).

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
letter_lowercase(X) --> a_lowercase(X).
letter_lowercase(X) --> b_lowercase(X).
letter_lowercase(X) --> c_lowercase(X).
letter_lowercase(X) --> d_lowercase(X).
letter_lowercase(X) --> e_lowercase(X).
letter_lowercase(X) --> f_lowercase(X).
letter_lowercase(X) --> g_lowercase(X).
letter_lowercase(X) --> h_lowercase(X).
letter_lowercase(X) --> i_lowercase(X).
letter_lowercase(X) --> j_lowercase(X).
letter_lowercase(X) --> k_lowercase(X).
letter_lowercase(X) --> l_lowercase(X).
letter_lowercase(X) --> m_lowercase(X).
letter_lowercase(X) --> n_lowercase(X).
letter_lowercase(X) --> o_lowercase(X).
letter_lowercase(X) --> p_lowercase(X).
letter_lowercase(X) --> q_lowercase(X).
letter_lowercase(X) --> r_lowercase(X).
letter_lowercase(X) --> s_lowercase(X).
letter_lowercase(X) --> t_lowercase(X).
letter_lowercase(X) --> u_lowercase(X).
letter_lowercase(X) --> v_lowercase(X).
letter_lowercase(X) --> w_lowercase(X).
letter_lowercase(X) --> x_lowercase(X).
letter_lowercase(X) --> y_lowercase(X).
letter_lowercase(X) --> z_lowercase(X).

letter_uppercase --> a_uppercase.
letter_uppercase --> b_uppercase.
letter_uppercase --> c_uppercase.
letter_uppercase --> d_uppercase.
letter_uppercase --> e_uppercase.
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
letter_uppercase(X) --> a_uppercase(X).
letter_uppercase(X) --> b_uppercase(X).
letter_uppercase(X) --> c_uppercase(X).
letter_uppercase(X) --> d_uppercase(X).
letter_uppercase(X) --> e_uppercase(X).
letter_uppercase(X) --> g_uppercase(X).
letter_uppercase(X) --> h_uppercase(X).
letter_uppercase(X) --> i_uppercase(X).
letter_uppercase(X) --> j_uppercase(X).
letter_uppercase(X) --> k_uppercase(X).
letter_uppercase(X) --> l_uppercase(X).
letter_uppercase(X) --> m_uppercase(X).
letter_uppercase(X) --> n_uppercase(X).
letter_uppercase(X) --> o_uppercase(X).
letter_uppercase(X) --> p_uppercase(X).
letter_uppercase(X) --> q_uppercase(X).
letter_uppercase(X) --> r_uppercase(X).
letter_uppercase(X) --> s_uppercase(X).
letter_uppercase(X) --> t_uppercase(X).
letter_uppercase(X) --> u_uppercase(X).
letter_uppercase(X) --> v_uppercase(X).
letter_uppercase(X) --> w_uppercase(X).
letter_uppercase(X) --> x_uppercase(X).
letter_uppercase(X) --> y_uppercase(X).
letter_uppercase(X) --> z_uppercase(X).

m --> m_lowercase.
m --> m_uppercase.
m(X) --> m_lowercase(X).
m(X) --> m_uppercase(X).

m_lowercase --> [109].
m_lowercase(109) --> [109].

m_uppercase --> [77].
m_uppercase(77) --> [77].

minus_sign --> hyphen_minus.
minus_sign(X) --> hyphen_minus(X).

n --> n_lowercase.
n --> n_uppercase.
n(X) --> n_lowercase(X).
n(X) --> n_uppercase(X).

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
o(X) --> o_lowercase(X).
o(X) --> o_uppercase(X).

o_lowercase --> [111].
o_lowercase(111) --> [111].

o_uppercase --> [79].
o_uppercase(79) --> [79].

one --> [49].
one(49) --> [49].

opening_bracket --> opening_curly_bracket.
opening_bracket --> opening_round_bracket.
opening_bracket --> opening_square_bracket.
opening_bracket(X) --> opening_curly_bracket(X).
opening_bracket(X) --> opening_round_bracket(X).
opening_bracket(X) --> opening_square_bracket(X).

opening_curly_bracket --> [123].
opening_curly_bracket(123) --> [123].

opening_round_bracket --> [40].
opening_round_bracket(40) --> [40].

opening_square_bracket --> [91].
opening_square_bracket(91) --> [91].

p --> p_lowercase.
p --> p_uppercase.
p(X) --> p_lowercase(X).
p(X) --> p_uppercase(X).

p_lowercase --> [112].
p_lowercase(112) --> [112].

p_uppercase --> [80].
p_uppercase(80) --> [80].

percent_sign --> [37].
percent_sign(37) --> [37].

plus_sign --> [43].
plus_sign(43) --> [43].

positive_acknowledgement --> [6].
positive_acknowledgement(6) --> [6].

dcg_print --> dcg_graph.
dcg_print --> space.
dcg_print(X) --> dcg_graph(X).
dcg_print(X) --> space(X).

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
punctuation(X) --> ampersand(X).
punctuation(X) --> apostrophe(X).
punctuation(X) --> asterisk(X).
punctuation(X) --> at_sign(X).
punctuation(X) --> bracket(X).
punctuation(X) --> caret(X).
punctuation(X) --> colon(X).
punctuation(X) --> comma(X).
punctuation(X) --> dollar_sign(X).
punctuation(X) --> dot(X).
punctuation(X) --> double_quote(X).
punctuation(X) --> equals_sign(X).
punctuation(X) --> exclamation_mark(X).
punctuation(X) --> grave_accent(X).
punctuation(X) --> greater_than_sign(X).
punctuation(X) --> hyphen_minus(X).
punctuation(X) --> less_than_sign(X).
punctuation(X) --> number_sign(X).
punctuation(X) --> percent_sign(X).
punctuation(X) --> plus_sign(X).
punctuation(X) --> question_mark(X).
punctuation(X) --> semi_colon(X).
punctuation(X) --> slash(X).
punctuation(X) --> tilde(X).
punctuation(X) --> underscore(X).
punctuation(X) --> vertical_bar(X).

q --> q_lowercase.
q --> q_uppercase.
q(X) --> q_lowercase(X).
q(X) --> q_uppercase(X).

q_lowercase --> [113].
q_lowercase(113) --> [113].

q_uppercase --> [81].
q_uppercase(81) --> [81].

question_mark --> [63].
question_mark(63) --> [63].

r --> r_lowercase.
r --> r_uppercase.
r(X) --> r_lowercase(X).
r(X) --> r_uppercase(X).

r_lowercase --> [114].
r_lowercase(114) --> [114].

r_uppercase --> [82].
r_uppercase(82) --> [82].

record_separator --> [30].
record_separator(30) --> [30].

round_bracket --> closing_round_bracket.
round_bracket --> opening_round_bracket.
round_bracket(X) --> closing_round_bracket(X).
round_bracket(X) --> opening_round_bracket(X).

s --> s_lowercase.
s --> s_uppercase.
s(X) --> s_lowercase(X).
s(X) --> s_uppercase(X).

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
shift(X) --> shift_in(X).
shift(X) --> shift_out(X).

shift_in --> [15].
shift_in(15) --> [15].

shift_out --> [14].
shift_out(14) --> [14].

single_quote --> apostrophe.
single_quote(X) --> apostrophe(X).

six --> [54].
six(54) --> [54].

slash --> backslash.
slash --> forward_slash.
slash(X) --> backslash(X).
slash(X) --> forward_slash(X).

space --> [32].
space(32) --> [32].

square_bracket --> closing_square_bracket.
square_bracket --> opening_square_bracket.
square_bracket(X) --> closing_square_bracket(X).
square_bracket(X) --> opening_square_bracket(X).

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
t(X) --> t_lowercase(X).
t(X) --> t_uppercase(X).

t_lowercase --> [116].
t_lowercase(116) --> [116].

t_uppercase --> [84].
t_uppercase(84) --> [84].

dcg_tab --> horizontal_tab.
dcg_tab --> vertical_tab.
dcg_tab(X) --> horizontal_tab(X).
dcg_tab(X) --> vertical_tab(X).

three --> [51].
three(51) --> [51].

tilde --> [126].
tilde(126) --> [126].

two --> [50].
two(50) --> [50].

u --> u_lowercase.
u --> u_uppercase.
u(X) --> u_lowercase(X).
u(X) --> u_uppercase(X).

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
v(X) --> v_lowercase(X).
v(X) --> v_uppercase(X).

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
w(X) --> w_lowercase(X).
w(X) --> w_uppercase(X).

w_lowercase --> [119].
w_lowercase(119) --> [119].

w_uppercase --> [87].
w_uppercase(87) --> [87].

dcg_white --> end_of_line.
dcg_white --> form_feed.
dcg_white --> space.
dcg_white --> dcg_tab.
dcg_white(X) --> end_of_line(X).
dcg_white(X) --> form_feed(X).
dcg_white(X) --> space(X).
dcg_white(X) --> dcg_tab(X).

x --> x_lowercase.
x --> x_uppercase.
x(X) --> x_lowercase(X).
x(X) --> x_uppercase(X).

x_lowercase --> [120].
x_lowercase(120) --> [120].

x_uppercase --> [88].
x_uppercase(88) --> [88].

y --> y_lowercase.
y --> y_uppercase.
y(X) --> y_lowercase(X).
y(X) --> y_uppercase(X).

y_lowercase --> [121].
y_lowercase(121) --> [121].

y_uppercase --> [89].
y_uppercase(89) --> [89].

z --> z_lowercase.
z --> z_uppercase.
z(X) --> z_lowercase(X).
z(X) --> z_uppercase(X).

z_lowercase --> [122].
z_lowercase(122) --> [122].

z_uppercase --> [90].
z_uppercase(90) --> [90].

zero --> [48].
zero(48) --> [48].
