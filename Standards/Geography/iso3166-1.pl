:- module(
  'iso3166-1',
  [
    'iso3166-1'/3 % ?Code:atom
                  % ?Name:atom
                  % ?Resource:uri
  ]
).

/** <module> ISO 3166-1

Suopport for the ISO 3166-1 country code standard.

@author Wouter Beek
@version 2013/01, 2013/06
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('iso3166-1', 'http://lexvo.org/id/iso3166/').

:- rdf_meta('iso3166-1'(?,?,r)).



'iso3166-1'(ad, 'Andorra', 'iso3166-1':'AD').
'iso3166-1'(ae, 'United arab emirates', 'iso3166-1':'AE').
'iso3166-1'(af, 'Afghanistan', 'iso3166-1':'AF').
'iso3166-1'(ag, 'Antigua and barbuda', 'iso3166-1':'AG').
'iso3166-1'(ai, 'Anguilla', 'iso3166-1':'AI').
'iso3166-1'(al, 'Albania', 'iso3166-1':'AL').
'iso3166-1'(am, 'Armenia', 'iso3166-1':'AM').
'iso3166-1'(ao, 'Angola', 'iso3166-1':'AO').
'iso3166-1'(aq, 'Antarctica', 'iso3166-1':'AQ').
'iso3166-1'(ar, 'Argentina', 'iso3166-1':'AR').
'iso3166-1'(as, 'American samoa', 'iso3166-1':'AS').
'iso3166-1'(at, 'Austria', 'iso3166-1':'AT').
'iso3166-1'(au, 'Australia', 'iso3166-1':'AU').
'iso3166-1'(aw, 'Aruba', 'iso3166-1':'AW').
'iso3166-1'(ax, 'Åland islands', 'iso3166-1':'AX').
'iso3166-1'(az, 'Azerbaijan', 'iso3166-1':'AZ').
'iso3166-1'(ba, 'Bosnia and herzegovina', 'iso3166-1':'BA').
'iso3166-1'(bb, 'Barbados', 'iso3166-1':'BB').
'iso3166-1'(bd, 'Bangladesh', 'iso3166-1':'BD').
'iso3166-1'(be, 'Belgium', 'iso3166-1':'BE').
'iso3166-1'(bf, 'Burkina faso', 'iso3166-1':'BF').
'iso3166-1'(bg, 'Bulgaria', 'iso3166-1':'BG').
'iso3166-1'(bh, 'Bahrain', 'iso3166-1':'BH').
'iso3166-1'(bi, 'Burundi', 'iso3166-1':'BI').
'iso3166-1'(bj, 'Benin', 'iso3166-1':'BJ').
'iso3166-1'(bl, 'Saint barthélemy', 'iso3166-1':'BL').
'iso3166-1'(bm, 'Bermuda', 'iso3166-1':'BM').
'iso3166-1'(bn, 'Brunei darussalam', 'iso3166-1':'BN').
'iso3166-1'(bo, 'Bolivia, plurinational state of', 'iso3166-1':'BO').
'iso3166-1'(bq, 'Bonaire, sint eustatius and saba', 'iso3166-1':'BQ').
'iso3166-1'(br, 'Brazil', 'iso3166-1':'BR').
'iso3166-1'(bs, 'Bahamas', 'iso3166-1':'BS').
'iso3166-1'(bt, 'Bhutan', 'iso3166-1':'BT').
'iso3166-1'(bv, 'Bouvet island', 'iso3166-1':'BV').
'iso3166-1'(bw, 'Botswana', 'iso3166-1':'BW').
'iso3166-1'(by, 'Belarus', 'iso3166-1':'BY').
'iso3166-1'(bz, 'Belize', 'iso3166-1':'BZ').
'iso3166-1'(ca, 'Canada', 'iso3166-1':'CA').
'iso3166-1'(cc, 'Cocos (keeling) islands', 'iso3166-1':'CC').
'iso3166-1'(cd, 'Congo, the democratic republic of the', 'iso3166-1':'CD').
'iso3166-1'(cf, 'Central african republic', 'iso3166-1':'CF').
'iso3166-1'(cg, 'Congo', 'iso3166-1':'CG').
'iso3166-1'(ch, 'Switzerland', 'iso3166-1':'CH').
'iso3166-1'(ci, 'Côte d\'ivoire', 'iso3166-1':'CI').
'iso3166-1'(ck, 'Cook islands', 'iso3166-1':'CK').
'iso3166-1'(cl, 'Chile', 'iso3166-1':'CL').
'iso3166-1'(cm, 'Cameroon', 'iso3166-1':'CM').
'iso3166-1'(cn, 'China', 'iso3166-1':'CN').
'iso3166-1'(co, 'Colombia', 'iso3166-1':'CO').
'iso3166-1'(cr, 'Costa rica', 'iso3166-1':'CR').
'iso3166-1'(cu, 'Cuba', 'iso3166-1':'CU').
'iso3166-1'(cv, 'Cape verde', 'iso3166-1':'CV').
'iso3166-1'(cw, 'Curaçao', 'iso3166-1':'CW').
'iso3166-1'(cx, 'Christmas island', 'iso3166-1':'CX').
'iso3166-1'(cy, 'Cyprus', 'iso3166-1':'CY').
'iso3166-1'(cz, 'Czech republic', 'iso3166-1':'CZ').
'iso3166-1'(de, 'Germany', 'iso3166-1':'DE').
'iso3166-1'(dj, 'Djibouti', 'iso3166-1':'DJ').
'iso3166-1'(dk, 'Denmark', 'iso3166-1':'DK').
'iso3166-1'(dm, 'Dominica', 'iso3166-1':'DM').
'iso3166-1'(do, 'Dominican republic', 'iso3166-1':'DO').
'iso3166-1'(dz, 'Algeria', 'iso3166-1':'DZ').
'iso3166-1'(ec, 'Ecuador', 'iso3166-1':'EC').
'iso3166-1'(ee, 'Estonia', 'iso3166-1':'EE').
'iso3166-1'(eg, 'Egypt', 'iso3166-1':'EG').
'iso3166-1'(eh, 'Western sahara', 'iso3166-1':'EH').
'iso3166-1'(er, 'Eritrea', 'iso3166-1':'ER').
'iso3166-1'(es, 'Spain', 'iso3166-1':'ES').
'iso3166-1'(et, 'Ethiopia', 'iso3166-1':'ET').
'iso3166-1'(fi, 'Finland', 'iso3166-1':'FI').
'iso3166-1'(fj, 'Fiji', 'iso3166-1':'FJ').
'iso3166-1'(fk, 'Falkland islands (malvinas)', 'iso3166-1':'FK').
'iso3166-1'(fm, 'Micronesia, federated states of', 'iso3166-1':'FM').
'iso3166-1'(fo, 'Faroe islands', 'iso3166-1':'FO').
'iso3166-1'(fr, 'France', 'iso3166-1':'FR').
'iso3166-1'(ga, 'Gabon', 'iso3166-1':'GA').
'iso3166-1'(gb, 'United kingdom', 'iso3166-1':'GB').
'iso3166-1'(gd, 'Grenada', 'iso3166-1':'GD').
'iso3166-1'(ge, 'Georgia', 'iso3166-1':'GE').
'iso3166-1'(gf, 'French guiana', 'iso3166-1':'GF').
'iso3166-1'(gg, 'Guernsey', 'iso3166-1':'GG').
'iso3166-1'(gh, 'Ghana', 'iso3166-1':'GH').
'iso3166-1'(gi, 'Gibraltar', 'iso3166-1':'GI').
'iso3166-1'(gl, 'Greenland', 'iso3166-1':'GL').
'iso3166-1'(gm, 'Gambia', 'iso3166-1':'GM').
'iso3166-1'(gn, 'Guinea', 'iso3166-1':'GN').
'iso3166-1'(gp, 'Guadeloupe', 'iso3166-1':'GP').
'iso3166-1'(gq, 'Equatorial guinea', 'iso3166-1':'GQ').
'iso3166-1'(gr, 'Greece', 'iso3166-1':'GR').
'iso3166-1'(gs, 'South georgia and the south sandwich islands', 'iso3166-1':'GS').
'iso3166-1'(gt, 'Guatemala', 'iso3166-1':'GT').
'iso3166-1'(gu, 'Guam', 'iso3166-1':'GU').
'iso3166-1'(gw, 'Guinea-bissau', 'iso3166-1':'GW').
'iso3166-1'(gy, 'Guyana', 'iso3166-1':'GY').
'iso3166-1'(hk, 'Hong kong', 'iso3166-1':'HK').
'iso3166-1'(hm, 'Heard island and mcdonald islands', 'iso3166-1':'HM').
'iso3166-1'(hn, 'Honduras', 'iso3166-1':'HN').
'iso3166-1'(hr, 'Croatia', 'iso3166-1':'HR').
'iso3166-1'(ht, 'Haiti', 'iso3166-1':'HT').
'iso3166-1'(hu, 'Hungary', 'iso3166-1':'HU').
'iso3166-1'(id, 'Indonesia', 'iso3166-1':'ID').
'iso3166-1'(ie, 'Ireland', 'iso3166-1':'IE').
'iso3166-1'(il, 'Israel', 'iso3166-1':'IL').
'iso3166-1'(im, 'Isle of man', 'iso3166-1':'IM').
'iso3166-1'(in, 'India', 'iso3166-1':'IN').
'iso3166-1'(io, 'British indian ocean territory', 'iso3166-1':'IO').
'iso3166-1'(iq, 'Iraq', 'iso3166-1':'IQ').
'iso3166-1'(ir, 'Iran, islamic republic of', 'iso3166-1':'IR').
'iso3166-1'(is, 'Iceland', 'iso3166-1':'IS').
'iso3166-1'(it, 'Italy', 'iso3166-1':'IT').
'iso3166-1'(je, 'Jersey', 'iso3166-1':'JE').
'iso3166-1'(jm, 'Jamaica', 'iso3166-1':'JM').
'iso3166-1'(jo, 'Jordan', 'iso3166-1':'JO').
'iso3166-1'(jp, 'Japan', 'iso3166-1':'JP').
'iso3166-1'(ke, 'Kenya', 'iso3166-1':'KE').
'iso3166-1'(kg, 'Kyrgyzstan', 'iso3166-1':'KG').
'iso3166-1'(kh, 'Cambodia', 'iso3166-1':'KH').
'iso3166-1'(ki, 'Kiribati', 'iso3166-1':'KI').
'iso3166-1'(km, 'Comoros', 'iso3166-1':'KM').
'iso3166-1'(kn, 'Saint kitts and nevis', 'iso3166-1':'KN').
'iso3166-1'(kp, 'Korea, democratic people\'s republic of', 'iso3166-1':'KP').
'iso3166-1'(kr, 'Korea, republic of', 'iso3166-1':'KR').
'iso3166-1'(kw, 'Kuwait', 'iso3166-1':'KW').
'iso3166-1'(ky, 'Cayman islands', 'iso3166-1':'KY').
'iso3166-1'(kz, 'Kazakhstan', 'iso3166-1':'KZ').
'iso3166-1'(la, 'Lao people\'s democratic republic', 'iso3166-1':'LA').
'iso3166-1'(lb, 'Lebanon', 'iso3166-1':'LB').
'iso3166-1'(lc, 'Saint lucia', 'iso3166-1':'LC').
'iso3166-1'(li, 'Liechtenstein', 'iso3166-1':'LI').
'iso3166-1'(lk, 'Sri lanka', 'iso3166-1':'LK').
'iso3166-1'(lr, 'Liberia', 'iso3166-1':'LR').
'iso3166-1'(ls, 'Lesotho', 'iso3166-1':'LS').
'iso3166-1'(lt, 'Lithuania', 'iso3166-1':'LT').
'iso3166-1'(lu, 'Luxembourg', 'iso3166-1':'LU').
'iso3166-1'(lv, 'Latvia', 'iso3166-1':'LV').
'iso3166-1'(ly, 'Libya', 'iso3166-1':'LY').
'iso3166-1'(ma, 'Morocco', 'iso3166-1':'MA').
'iso3166-1'(mc, 'Monaco', 'iso3166-1':'MC').
'iso3166-1'(md, 'Moldova, republic of', 'iso3166-1':'MD').
'iso3166-1'(me, 'Montenegro', 'iso3166-1':'ME').
'iso3166-1'(mf, 'Saint martin (french part)', 'iso3166-1':'MF').
'iso3166-1'(mg, 'Madagascar', 'iso3166-1':'MG').
'iso3166-1'(mh, 'Marshall islands', 'iso3166-1':'MH').
'iso3166-1'(mk, 'Macedonia, the former yugoslav republic of', 'iso3166-1':'MK').
'iso3166-1'(ml, 'Mali', 'iso3166-1':'ML').
'iso3166-1'(mm, 'Myanmar', 'iso3166-1':'MM').
'iso3166-1'(mn, 'Mongolia', 'iso3166-1':'MN').
'iso3166-1'(mo, 'Macao', 'iso3166-1':'MO').
'iso3166-1'(mp, 'Northern mariana islands', 'iso3166-1':'MP').
'iso3166-1'(mq, 'Martinique', 'iso3166-1':'MQ').
'iso3166-1'(mr, 'Mauritania', 'iso3166-1':'MR').
'iso3166-1'(ms, 'Montserrat', 'iso3166-1':'MS').
'iso3166-1'(mt, 'Malta', 'iso3166-1':'MT').
'iso3166-1'(mu, 'Mauritius', 'iso3166-1':'MU').
'iso3166-1'(mv, 'Maldives', 'iso3166-1':'MV').
'iso3166-1'(mw, 'Malawi', 'iso3166-1':'MW').
'iso3166-1'(mx, 'Mexico', 'iso3166-1':'MX').
'iso3166-1'(my, 'Malaysia', 'iso3166-1':'MY').
'iso3166-1'(mz, 'Mozambique', 'iso3166-1':'MZ').
'iso3166-1'(na, 'Namibia', 'iso3166-1':'NA').
'iso3166-1'(nc, 'New caledonia', 'iso3166-1':'NC').
'iso3166-1'(ne, 'Niger', 'iso3166-1':'NE').
'iso3166-1'(nf, 'Norfolk island', 'iso3166-1':'NF').
'iso3166-1'(ng, 'Nigeria', 'iso3166-1':'NG').
'iso3166-1'(ni, 'Nicaragua', 'iso3166-1':'NI').
'iso3166-1'(nl, 'Netherlands', 'iso3166-1':'NL').
'iso3166-1'(no, 'Norway', 'iso3166-1':'NO').
'iso3166-1'(np, 'Nepal', 'iso3166-1':'NP').
'iso3166-1'(nr, 'Nauru', 'iso3166-1':'NR').
'iso3166-1'(nu, 'Niue', 'iso3166-1':'NU').
'iso3166-1'(nz, 'New zealand', 'iso3166-1':'NZ').
'iso3166-1'(om, 'Oman', 'iso3166-1':'OM').
'iso3166-1'(pa, 'Panama', 'iso3166-1':'PA').
'iso3166-1'(pe, 'Peru', 'iso3166-1':'PE').
'iso3166-1'(pf, 'French polynesia', 'iso3166-1':'PF').
'iso3166-1'(pg, 'Papua new guinea', 'iso3166-1':'PG').
'iso3166-1'(ph, 'Philippines', 'iso3166-1':'PH').
'iso3166-1'(pk, 'Pakistan', 'iso3166-1':'PK').
'iso3166-1'(pl, 'Poland', 'iso3166-1':'PL').
'iso3166-1'(pm, 'Saint pierre and miquelon', 'iso3166-1':'PM').
'iso3166-1'(pn, 'Pitcairn', 'iso3166-1':'PN').
'iso3166-1'(pr, 'Puerto rico', 'iso3166-1':'PR').
'iso3166-1'(ps, 'Palestinian territory, occupied', 'iso3166-1':'PS').
'iso3166-1'(pt, 'Portugal', 'iso3166-1':'PT').
'iso3166-1'(pw, 'Palau', 'iso3166-1':'PW').
'iso3166-1'(py, 'Paraguay', 'iso3166-1':'PY').
'iso3166-1'(qa, 'Qatar', 'iso3166-1':'QA').
'iso3166-1'(re, 'Réunion', 'iso3166-1':'RE').
'iso3166-1'(ro, 'Romania', 'iso3166-1':'RO').
'iso3166-1'(rs, 'Serbia', 'iso3166-1':'RS').
'iso3166-1'(ru, 'Russian federation', 'iso3166-1':'RU').
'iso3166-1'(rw, 'Rwanda', 'iso3166-1':'RW').
'iso3166-1'(sa, 'Saudi arabia', 'iso3166-1':'SA').
'iso3166-1'(sb, 'Solomon islands', 'iso3166-1':'SB').
'iso3166-1'(sc, 'Seychelles', 'iso3166-1':'SC').
'iso3166-1'(sd, 'Sudan', 'iso3166-1':'SD').
'iso3166-1'(se, 'Sweden', 'iso3166-1':'SE').
'iso3166-1'(sg, 'Singapore', 'iso3166-1':'SG').
'iso3166-1'(sh, 'Saint helena, ascension and tristan da cunha', 'iso3166-1':'SH').
'iso3166-1'(si, 'Slovenia', 'iso3166-1':'SI').
'iso3166-1'(sj, 'Svalbard and jan mayen', 'iso3166-1':'SJ').
'iso3166-1'(sk, 'Slovakia', 'iso3166-1':'SK').
'iso3166-1'(sl, 'Sierra leone', 'iso3166-1':'SL').
'iso3166-1'(sm, 'San marino', 'iso3166-1':'SM').
'iso3166-1'(sn, 'Senegal', 'iso3166-1':'SN').
'iso3166-1'(so, 'Somalia', 'iso3166-1':'SO').
'iso3166-1'(sr, 'Suriname', 'iso3166-1':'SR').
'iso3166-1'(ss, 'South sudan', 'iso3166-1':'SS').
'iso3166-1'(st, 'Sao tome and principe', 'iso3166-1':'ST').
'iso3166-1'(sv, 'El salvador', 'iso3166-1':'SV').
'iso3166-1'(sx, 'Sint maarten (dutch part)', 'iso3166-1':'SX').
'iso3166-1'(sy, 'Syrian arab republic', 'iso3166-1':'SY').
'iso3166-1'(sz, 'Swaziland', 'iso3166-1':'SZ').
'iso3166-1'(tc, 'Turks and caicos islands', 'iso3166-1':'TC').
'iso3166-1'(td, 'Chad', 'iso3166-1':'TD').
'iso3166-1'(tf, 'French southern territories', 'iso3166-1':'TF').
'iso3166-1'(tg, 'Togo', 'iso3166-1':'TG').
'iso3166-1'(th, 'Thailand', 'iso3166-1':'TH').
'iso3166-1'(tj, 'Tajikistan', 'iso3166-1':'TJ').
'iso3166-1'(tk, 'Tokelau', 'iso3166-1':'TK').
'iso3166-1'(tl, 'Timor-leste', 'iso3166-1':'TL').
'iso3166-1'(tm, 'Turkmenistan', 'iso3166-1':'TM').
'iso3166-1'(tn, 'Tunisia', 'iso3166-1':'TN').
'iso3166-1'(to, 'Tonga', 'iso3166-1':'TO').
'iso3166-1'(tr, 'Turkey', 'iso3166-1':'TR').
'iso3166-1'(tt, 'Trinidad and tobago', 'iso3166-1':'TT').
'iso3166-1'(tv, 'Tuvalu', 'iso3166-1':'TV').
'iso3166-1'(tw, 'Taiwan, province of china', 'iso3166-1':'TW').
'iso3166-1'(tz, 'Tanzania, united republic of', 'iso3166-1':'TZ').
'iso3166-1'(ua, 'Ukraine', 'iso3166-1':'UA').
'iso3166-1'(ug, 'Uganda', 'iso3166-1':'UG').
'iso3166-1'(um, 'United states minor outlying islands', 'iso3166-1':'UM').
'iso3166-1'(us, 'United states', 'iso3166-1':'US').
'iso3166-1'(uy, 'Uruguay', 'iso3166-1':'UY').
'iso3166-1'(uz, 'Uzbekistan', 'iso3166-1':'UZ').
'iso3166-1'(va, 'Holy see (vatican city state)', 'iso3166-1':'VA').
'iso3166-1'(vc, 'Saint vincent and the grenadines', 'iso3166-1':'VC').
'iso3166-1'(ve, 'Venezuela, bolivarian republic of', 'iso3166-1':'VE').
'iso3166-1'(vg, 'Virgin islands, british', 'iso3166-1':'VG').
'iso3166-1'(vi, 'Virgin islands, u.s.', 'iso3166-1':'VI').
'iso3166-1'(vn, 'Viet nam', 'iso3166-1':'VN').
'iso3166-1'(vu, 'Vanuatu', 'iso3166-1':'VU').
'iso3166-1'(wf, 'Wallis and futuna', 'iso3166-1':'WF').
'iso3166-1'(ws, 'Samoa', 'iso3166-1':'WS').
'iso3166-1'(ye, 'Yemen', 'iso3166-1':'YE').
'iso3166-1'(yt, 'Mayotte', 'iso3166-1':'YT').
'iso3166-1'(za, 'South africa', 'iso3166-1':'ZA').
'iso3166-1'(zm, 'Zambia', 'iso3166-1':'ZM').
'iso3166-1'(zw, 'Zimbabwe', 'iso3166-1':'ZW').

