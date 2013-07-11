:- module(
  iso_639_1,
  [
    iso_639_1//1 % ?URI:atom
  ]
).

/** <module> ISO 639-1

The ISO 639-1 standard for language codes with Lexvo Semantic Web URIs.

# Table of supported languages

!!iso_639_1!!language!!2!!

# RE

The old, non-DCG representation looked like this:

~~~{.pl}
iso_639_1(iso_639_3:aar) --> "aa".
~~~

The following RE notation was used to replace this representation to
the DCG representation:
  * From: `iso_639_1\(([a-z]+), ([^)]+)\).`
  * To:   `iso_639_1\(\2\) --> "\1".`

@author Wouter Beek
@see http://www.sil.org/iso639-1/
@version 2013/01, 2013/06-2013/07
*/

:- use_module(library(semweb/rdf_db)). % For rdf_meta/1.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso_639_3, 'http://lexvo.org/id/iso639-3/').

:- rdf_meta(iso_639_1(r,?,?)).



%! iso_639_1(?Lexvo:uri)//

iso_639_1(iso_639_3:aar) --> "aa".
iso_639_1(iso_639_3:abk) --> "ab".
iso_639_1(iso_639_3:ave) --> "ae".
iso_639_1(iso_639_3:afr) --> "af".
iso_639_1(iso_639_3:aka) --> "ak".
iso_639_1(iso_639_3:amh) --> "am".
iso_639_1(iso_639_3:arg) --> "an".
iso_639_1(iso_639_3:ara) --> "ar".
iso_639_1(iso_639_3:asm) --> "as".
iso_639_1(iso_639_3:ava) --> "av".
iso_639_1(iso_639_3:aym) --> "ay".
iso_639_1(iso_639_3:aze) --> "az".
iso_639_1(iso_639_3:bak) --> "ba".
iso_639_1(iso_639_3:bel) --> "be".
iso_639_1(iso_639_3:bul) --> "bg".
iso_639_1(iso_639_3:bis) --> "bi".
iso_639_1(iso_639_3:bam) --> "bm".
iso_639_1(iso_639_3:ben) --> "bn".
iso_639_1(iso_639_3:bod) --> "bo".
iso_639_1(iso_639_3:bre) --> "br".
iso_639_1(iso_639_3:bos) --> "bs".
iso_639_1(iso_639_3:cat) --> "ca".
iso_639_1(iso_639_3:che) --> "ce".
iso_639_1(iso_639_3:cha) --> "ch".
iso_639_1(iso_639_3:cos) --> "co".
iso_639_1(iso_639_3:cre) --> "cr".
iso_639_1(iso_639_3:ces) --> "cs".
iso_639_1(iso_639_3:chu) --> "cu".
iso_639_1(iso_639_3:chv) --> "cv".
iso_639_1(iso_639_3:cym) --> "cy".
iso_639_1(iso_639_3:dan) --> "da".
iso_639_1(iso_639_3:deu) --> "de".
iso_639_1(iso_639_3:div) --> "dv".
iso_639_1(iso_639_3:dzo) --> "dz".
iso_639_1(iso_639_3:ewe) --> "ee".
iso_639_1(iso_639_3:ell) --> "el".
iso_639_1(iso_639_3:eng) --> "en".
iso_639_1(iso_639_3:epo) --> "eo".
iso_639_1(iso_639_3:spa) --> "es".
iso_639_1(iso_639_3:est) --> "et".
iso_639_1(iso_639_3:eus) --> "eu".
iso_639_1(iso_639_3:fas) --> "fa".
iso_639_1(iso_639_3:ful) --> "ff".
iso_639_1(iso_639_3:fin) --> "fi".
iso_639_1(iso_639_3:fij) --> "fj".
iso_639_1(iso_639_3:fao) --> "fo".
iso_639_1(iso_639_3:fra) --> "fr".
iso_639_1(iso_639_3:fry) --> "fy".
iso_639_1(iso_639_3:gle) --> "ga".
iso_639_1(iso_639_3:gla) --> "gd".
iso_639_1(iso_639_3:glg) --> "gl".
iso_639_1(iso_639_3:grn) --> "gn".
iso_639_1(iso_639_3:guj) --> "gu".
iso_639_1(iso_639_3:glv) --> "gv".
iso_639_1(iso_639_3:hau) --> "ha".
iso_639_1(iso_639_3:heb) --> "he".
iso_639_1(iso_639_3:hin) --> "hi".
iso_639_1(iso_639_3:hmo) --> "ho".
iso_639_1(iso_639_3:hrv) --> "hr".
iso_639_1(iso_639_3:hat) --> "ht".
iso_639_1(iso_639_3:hun) --> "hu".
iso_639_1(iso_639_3:hye) --> "hy".
iso_639_1(iso_639_3:her) --> "hz".
iso_639_1(iso_639_3:ina) --> "ia".
iso_639_1(iso_639_3:ind) --> "id".
iso_639_1(iso_639_3:ile) --> "ie".
iso_639_1(iso_639_3:ibo) --> "ig".
iso_639_1(iso_639_3:iii) --> "ii".
iso_639_1(iso_639_3:ipk) --> "ik".
iso_639_1(iso_639_3:ido) --> "io".
iso_639_1(iso_639_3:isl) --> "is".
iso_639_1(iso_639_3:ita) --> "it".
iso_639_1(iso_639_3:iku) --> "iu".
iso_639_1(iso_639_3:jpn) --> "ja".
iso_639_1(iso_639_3:jav) --> "jv".
iso_639_1(iso_639_3:kat) --> "ka".
iso_639_1(iso_639_3:kon) --> "kg".
iso_639_1(iso_639_3:kik) --> "ki".
iso_639_1(iso_639_3:kua) --> "kj".
iso_639_1(iso_639_3:kaz) --> "kk".
iso_639_1(iso_639_3:kal) --> "kl".
iso_639_1(iso_639_3:khm) --> "km".
iso_639_1(iso_639_3:kan) --> "kn".
iso_639_1(iso_639_3:kor) --> "ko".
iso_639_1(iso_639_3:kau) --> "kr".
iso_639_1(iso_639_3:kas) --> "ks".
iso_639_1(iso_639_3:kur) --> "ku".
iso_639_1(iso_639_3:kom) --> "kv".
iso_639_1(iso_639_3:cor) --> "kw".
iso_639_1(iso_639_3:kir) --> "ky".
iso_639_1(iso_639_3:lat) --> "la".
iso_639_1(iso_639_3:ltz) --> "lb".
iso_639_1(iso_639_3:lug) --> "lg".
iso_639_1(iso_639_3:lim) --> "li".
iso_639_1(iso_639_3:lin) --> "ln".
iso_639_1(iso_639_3:lao) --> "lo".
iso_639_1(iso_639_3:lit) --> "lt".
iso_639_1(iso_639_3:lub) --> "lu".
iso_639_1(iso_639_3:lav) --> "lv".
iso_639_1(iso_639_3:mlg) --> "mg".
iso_639_1(iso_639_3:mah) --> "mh".
iso_639_1(iso_639_3:mri) --> "mi".
iso_639_1(iso_639_3:mkd) --> "mk".
iso_639_1(iso_639_3:mal) --> "ml".
iso_639_1(iso_639_3:mon) --> "mn".
iso_639_1(iso_639_3:mar) --> "mr".
iso_639_1(iso_639_3:msa) --> "ms".
iso_639_1(iso_639_3:mlt) --> "mt".
iso_639_1(iso_639_3:mya) --> "my".
iso_639_1(iso_639_3:nau) --> "na".
iso_639_1(iso_639_3:nob) --> "nb".
iso_639_1(iso_639_3:nde) --> "nd".
iso_639_1(iso_639_3:nep) --> "ne".
iso_639_1(iso_639_3:ndo) --> "ng".
iso_639_1(iso_639_3:nld) --> "nl".
iso_639_1(iso_639_3:nno) --> "nn".
iso_639_1(iso_639_3:nor) --> "no".
iso_639_1(iso_639_3:nbl) --> "nr".
iso_639_1(iso_639_3:nav) --> "nv".
iso_639_1(iso_639_3:nya) --> "ny".
iso_639_1(iso_639_3:oci) --> "oc".
iso_639_1(iso_639_3:oji) --> "oj".
iso_639_1(iso_639_3:orm) --> "om".
iso_639_1(iso_639_3:ori) --> "or".
iso_639_1(iso_639_3:oss) --> "os".
iso_639_1(iso_639_3:pan) --> "pa".
iso_639_1(iso_639_3:pli) --> "pi".
iso_639_1(iso_639_3:pol) --> "pl".
iso_639_1(iso_639_3:pus) --> "ps".
iso_639_1(iso_639_3:por) --> "pt".
iso_639_1(iso_639_3:que) --> "qu".
iso_639_1(iso_639_3:roh) --> "rm".
iso_639_1(iso_639_3:run) --> "rn".
iso_639_1(iso_639_3:ron) --> "ro".
iso_639_1(iso_639_3:rus) --> "ru".
iso_639_1(iso_639_3:kin) --> "rw".
iso_639_1(iso_639_3:san) --> "sa".
iso_639_1(iso_639_3:srd) --> "sc".
iso_639_1(iso_639_3:snd) --> "sd".
iso_639_1(iso_639_3:sme) --> "se".
iso_639_1(iso_639_3:sag) --> "sg".
iso_639_1(iso_639_3:hbs) --> "sh".
iso_639_1(iso_639_3:sin) --> "si".
iso_639_1(iso_639_3:slk) --> "sk".
iso_639_1(iso_639_3:slv) --> "sl".
iso_639_1(iso_639_3:smo) --> "sm".
iso_639_1(iso_639_3:sna) --> "sn".
iso_639_1(iso_639_3:som) --> "so".
iso_639_1(iso_639_3:sqi) --> "sq".
iso_639_1(iso_639_3:srp) --> "sr".
iso_639_1(iso_639_3:ssw) --> "ss".
iso_639_1(iso_639_3:sot) --> "st".
iso_639_1(iso_639_3:sun) --> "su".
iso_639_1(iso_639_3:swe) --> "sv".
iso_639_1(iso_639_3:swa) --> "sw".
iso_639_1(iso_639_3:tam) --> "ta".
iso_639_1(iso_639_3:tel) --> "te".
iso_639_1(iso_639_3:tgk) --> "tg".
iso_639_1(iso_639_3:tha) --> "th".
iso_639_1(iso_639_3:tir) --> "ti".
iso_639_1(iso_639_3:tuk) --> "tk".
iso_639_1(iso_639_3:tgl) --> "tl".
iso_639_1(iso_639_3:tsn) --> "tn".
iso_639_1(iso_639_3:ton) --> "to".
iso_639_1(iso_639_3:tur) --> "tr".
iso_639_1(iso_639_3:tso) --> "ts".
iso_639_1(iso_639_3:tat) --> "tt".
iso_639_1(iso_639_3:twi) --> "tw".
iso_639_1(iso_639_3:tah) --> "ty".
iso_639_1(iso_639_3:uig) --> "ug".
iso_639_1(iso_639_3:ukr) --> "uk".
iso_639_1(iso_639_3:urd) --> "ur".
iso_639_1(iso_639_3:uzb) --> "uz".
iso_639_1(iso_639_3:ven) --> "ve".
iso_639_1(iso_639_3:vie) --> "vi".
iso_639_1(iso_639_3:vol) --> "vo".
iso_639_1(iso_639_3:wln) --> "wa".
iso_639_1(iso_639_3:wol) --> "wo".
iso_639_1(iso_639_3:xho) --> "xh".
iso_639_1(iso_639_3:yid) --> "yi".
iso_639_1(iso_639_3:yor) --> "yo".
iso_639_1(iso_639_3:zha) --> "za".
iso_639_1(iso_639_3:zho) --> "zh".
iso_639_1(iso_639_3:zul) --> "zu".
