:- module(
  iso_639_5,
  [
    iso_639_5//2 % ?Name:atom
                 % ?URI:atom
  ]
).

/** <module> ISO 639-5

The ISO 639-5 standard for language codes with Lexvo Semantic Web URIs.

@author Wouter Beek
@see http://www.loc.gov/standards/iso639-5/
@version 2013/01, 2013/06-2013/07
*/

:- use_module(library(semweb/rdf_db)). % For rdf_meta/1 directive.
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(iso_639_5, 'http://lexvo.org/id/iso639-5/').

:- rdf_meta(iso_639_5(?,r,?,?)).



iso_639_5('Afro-Asiatic', iso_639_5:afa) --> "afa".
iso_639_5('Algonquian', iso_639_5:alg) --> "alg".
iso_639_5('Atlantic-Congo', iso_639_5:alv) --> "alv".
iso_639_5('Apache', iso_639_5:apa) --> "apa".
iso_639_5('Alacalufan', iso_639_5:aqa) --> "aqa".
iso_639_5('Algic', iso_639_5:aql) --> "aql".
iso_639_5('Artificial', iso_639_5:art) --> "art".
iso_639_5('Athapascan', iso_639_5:ath) --> "ath".
iso_639_5('Arauan', iso_639_5:auf) --> "auf".
iso_639_5('Australian', iso_639_5:aus) --> "aus".
iso_639_5('Arawakan', iso_639_5:awd) --> "awd".
iso_639_5('Uto-Aztecan', iso_639_5:azc) --> "azc".
iso_639_5('Banda', iso_639_5:bad) --> "bad".
iso_639_5('Bamileke', iso_639_5:bai) --> "bai".
iso_639_5('Baltic', iso_639_5:bat) --> "bat".
iso_639_5('Berber', iso_639_5:ber) --> "ber".
iso_639_5('Bantu', iso_639_5:bnt) --> "bnt".
iso_639_5('Batak', iso_639_5:btk) --> "btk".
iso_639_5('Central American Indian', iso_639_5:cai) --> "cai".
iso_639_5('Caucasian', iso_639_5:cau) --> "cau".
iso_639_5('Chibchan', iso_639_5:cba) --> "cba".
iso_639_5('North Caucasian', iso_639_5:ccn) --> "ccn".
iso_639_5('South Caucasian', iso_639_5:ccs) --> "ccs".
iso_639_5('Chadic', iso_639_5:cdc) --> "cdc".
iso_639_5('Caddoan', iso_639_5:cdd) --> "cdd".
iso_639_5('Celtic', iso_639_5:cel) --> "cel".
iso_639_5('Chamic', iso_639_5:cmc) --> "cmc".
iso_639_5('Creoles and pidgins, English‑based', iso_639_5:cpe) --> "cpe".
iso_639_5('Creoles and pidgins, French‑based', iso_639_5:cpf) --> "cpf".
iso_639_5('Creoles and pidgins, Portuguese-based', iso_639_5:cpp) --> "cpp".
iso_639_5('Creoles and pidgins', iso_639_5:crp) --> "crp".
iso_639_5('Central Sudanic', iso_639_5:csu) --> "csu".
iso_639_5('Cushitic', iso_639_5:cus) --> "cus".
iso_639_5('Land Dayak', iso_639_5:day) --> "day".
iso_639_5('Mande', iso_639_5:dmn) --> "dmn".
iso_639_5('Dravidian', iso_639_5:dra) --> "dra".
iso_639_5('Egyptian', iso_639_5:egx) --> "egx".
iso_639_5('Eskimo-Aleut', iso_639_5:esx) --> "esx".
iso_639_5('Basque', iso_639_5:euq) --> "euq".
iso_639_5('Finno-Ugrian', iso_639_5:fiu) --> "fiu".
iso_639_5('Formosan', iso_639_5:fox) --> "fox".
iso_639_5('Germanic', iso_639_5:gem) --> "gem".
iso_639_5('East Germanic', iso_639_5:gme) --> "gme".
iso_639_5('North Germanic', iso_639_5:gmq) --> "gmq".
iso_639_5('West Germanic', iso_639_5:gmw) --> "gmw".
iso_639_5('Greek', iso_639_5:grk) --> "grk".
iso_639_5('Hmong-Mien', iso_639_5:hmx) --> "hmx".
iso_639_5('Hokan languages', iso_639_5:hok) --> "hok".
iso_639_5('Armenian', iso_639_5:hyx) --> "hyx".
iso_639_5('Indo-Iranian', iso_639_5:iir) --> "iir".
iso_639_5('Ijo', iso_639_5:ijo) --> "ijo".
iso_639_5('Indic', iso_639_5:inc) --> "inc".
iso_639_5('Indo-European', iso_639_5:ine) --> "ine".
iso_639_5('Iranian', iso_639_5:ira) --> "ira".
iso_639_5('Iroquoian', iso_639_5:iro) --> "iro".
iso_639_5('Italic', iso_639_5:itc) --> "itc".
iso_639_5('Japanese', iso_639_5:jpx) --> "jpx".
iso_639_5('Karen', iso_639_5:kar) --> "kar".
iso_639_5('Kordofanian', iso_639_5:kdo) --> "kdo".
iso_639_5('Khoisan', iso_639_5:khi) --> "khi".
iso_639_5('Kru', iso_639_5:kro) --> "kro".
iso_639_5('Austronesian', iso_639_5:map) --> "map".
iso_639_5('Mon-Khmer', iso_639_5:mkh) --> "mkh".
iso_639_5('Manobo', iso_639_5:mno) --> "mno".
iso_639_5('Munda', iso_639_5:mun) --> "mun".
iso_639_5('Mayan', iso_639_5:myn) --> "myn".
iso_639_5('Nahuatl', iso_639_5:nah) --> "nah".
iso_639_5('North American Indian', iso_639_5:nai) --> "nai".
iso_639_5('Trans-New Guinea', iso_639_5:ngf) --> "ngf".
iso_639_5('Niger-Kordofanian', iso_639_5:nic) --> "nic".
iso_639_5('Nubian', iso_639_5:nub) --> "nub".
iso_639_5('Oto-Manguean', iso_639_5:omq) --> "omq".
iso_639_5('Omotic', iso_639_5:omv) --> "omv".
iso_639_5('Otomian', iso_639_5:oto) --> "oto".
iso_639_5('Papuan', iso_639_5:paa) --> "paa".
iso_639_5('Philippine', iso_639_5:phi) --> "phi".
iso_639_5('Central Malayo-Polynesian', iso_639_5:plf) --> "plf".
iso_639_5('Malayo-Polynesian', iso_639_5:poz) --> "poz".
iso_639_5('Eastern Malayo-Polynesian', iso_639_5:pqe) --> "pqe".
iso_639_5('Western Malayo-Polynesian', iso_639_5:pqw) --> "pqw".
iso_639_5('Prakrit', iso_639_5:pra) --> "pra".
iso_639_5('Quechuan', iso_639_5:qwe) --> "qwe".
iso_639_5('Romance', iso_639_5:roa) --> "roa".
iso_639_5('South American Indian', iso_639_5:sai) --> "sai".
iso_639_5('Salishan', iso_639_5:sal) --> "sal".
iso_639_5('Eastern Sudanic', iso_639_5:sdv) --> "sdv".
iso_639_5('Semitic', iso_639_5:sem) --> "sem".
iso_639_5('sign', iso_639_5:sgn) --> "sgn".
iso_639_5('Siouan', iso_639_5:sio) --> "sio".
iso_639_5('Sino-Tibetan', iso_639_5:sit) --> "sit".
iso_639_5('Slavic', iso_639_5:sla) --> "sla".
iso_639_5('Sami', iso_639_5:smi) --> "smi".
iso_639_5('Songhai', iso_639_5:son) --> "son".
iso_639_5('Albanian', iso_639_5:sqj) --> "sqj".
iso_639_5('Nilo-Saharan', iso_639_5:ssa) --> "ssa".
iso_639_5('Samoyedic', iso_639_5:syd) --> "syd".
iso_639_5('Tai', iso_639_5:tai) --> "tai".
iso_639_5('Tibeto-Burman', iso_639_5:tbq) --> "tbq".
iso_639_5('Turkic', iso_639_5:trk) --> "trk".
iso_639_5('Tupi', iso_639_5:tup) --> "tup".
iso_639_5('Altaic', iso_639_5:tut) --> "tut".
iso_639_5('Tungus', iso_639_5:tuw) --> "tuw".
iso_639_5('Uralic', iso_639_5:urj) --> "urj".
iso_639_5('Wakashan', iso_639_5:wak) --> "wak".
iso_639_5('Sorbian', iso_639_5:wen) --> "wen".
iso_639_5('Mongolian', iso_639_5:xgn) --> "xgn".
iso_639_5('Na-Dene', iso_639_5:xnd) --> "xnd".
iso_639_5('Yupik', iso_639_5:ypk) --> "ypk".
iso_639_5('Chinese', iso_639_5:zhx) --> "zhx".
iso_639_5('East Slavic', iso_639_5:zle) --> "zle".
iso_639_5('South Slavic', iso_639_5:zls) --> "zls".
iso_639_5('West Slavic', iso_639_5:zlw) --> "zlw".
iso_639_5('Zande', iso_639_5:znd) --> "znd".

