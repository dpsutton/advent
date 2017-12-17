(ns advent.day9data
  (:require [clojure.string :as str]))

(def data "{{{{{{<{\"\"!!!>{>},{<i!>u!>!!\"{!!o}!>,<!u!>,<!!!!>}},{<{!!!>},<,'}!!!ou!>!>,<!ee!!},!!!>},<u>}},{{<,!>},<<!!!!!!!!{,o!!u!'!>,<,<!!!!u>},{{{<a'i<!!!>\"!!!!i!>!>},<<\">,{<}o\"<!>,<\"!>,<!!!>!!!>,!>,<u!>,<o'!!!>}{>}},{<!!!>u>,{}}},{<,\"!>!>oe!,!>,<\"u}u>},{{<<!>,<u!!!!!!!>a!u{{uo{!}!!!>,!>!>,<!!!><!i>}}}},{{{},<}!!u!}!!!!u!!!>},<,!>},<!>},<',}!>!!!>!!<u>},{<'a!u\"!!!>!>o\"!u!>},<\"aao>},{<,{\"!!!>e\">,{<!},\"!>,<ou!!!!!!!>o!>,<,o!>,<!>!!!u!>},<!!!!!>},<>}}}},{{{{{<,!\"u!>},<,a!!>}},{<!!!>!!!!}e',!!!>\"!!u>},{{<!>},<ao!>a!!!!e!>},<e>},<>}},{{<o!!!>{u!>!>!>,<!uoa!!ooi'e!!'<!!!>>,{{<!!!!!>,<{e!>,<eo<ao'},!>o!!!>>}}},{{{{<<,!>,<}'!!!!!o}!!o>},{{<!{!!i>},{<!>,<!!!i<,e\"eo!>,<>}}}},{},{{},{{<o}{a<e!!!>\"!e!>},<,}!>,<!!!>i!>>},<u<!'!!!>!!o>}}},{{<a!>},<{}!>,<oie\"}a!>>},{<'<<u!a>}}}},{{{<!>},<'!>,\"!e'!!i}<!\"!!}!>u{>,{<i<!>},<!!!>}!>},<o{a!uiou\">}},{{<!>},<!!<'ei<!>},<!>,<{!!!>i!>},<o!!!>\"!!!!ao>,<>}}},{{{{<!!<!,>}},{<!!!>!!<\"ou!!!!!>!!!>,<!!!>!!i!>},<<>,<ii!!,>}},{},{{}}},{{{<!>,<i!<{}o}!!!!ue'>},{{<!!{!!!!}\"'!>,<{!!>,{<{!!!>o,{!!\"!!!>,!!!>>}},{{},{<\"'!>!!<\"i!!!!!>{>}}},{<{!>,<!!!>>,{}}},{{},{{{{},{<ei!!o<'!!!>\"<\"!>},<!!u\"'o>}},{<a!!!!!>>},{<a<o\"\"}!i!!!''\",!u\"!!!>>,{}}},{{{{<oi!>,<}!!'<!u!!!>!!!>'!!e!!a!>\">}}},{<<e!a!!!!!!!!o!!!>!>,<i!>},<\"'i<a!!!>>},{{{<a>}}}}}},{{{{<!!ee!!>}},<{!}!!o!>,<!!eu<!>},<}'!!!>},<>},{},{<eeo!!>,<!>}io!!!>\"!>},<!>!!i',>}}}},{{{{{{},{<\"au}{a!!}!!ou!!i>,<e>}}}}},{{{<a>}},{{{<!!!>,<<a!!<>},{<!>,<}!>},<o!>},<!!\"!!<,!!e''!>,<!>},<!>,<>}},{<a<\"<u!,\"!!<,a>}}},{{{<<uo<!>},<>,<!!\"!!<!!,i!>},<!>},<'!!!>\"!>!!e''!>,<!!,!!a>},{<'{!!!!}\"i<!>},<!o!a!!!>>,{}}},{<{{!>i!<!>!>,<ie,!!}\"'o'!>!!!>},<>,{<}!!\"!>}>}},{{{{<>},<iou!>!>},<o!!ee!>,<>},<>},{<u\",''u'!>},<!>},<\"!>},<u>,{<!!i!>,<!!!>}}!!!>a!>},<{e>}},{}}}}},{},{{{{{<!>},<!>,<''!>,<uu\"uo!>},<!>,<u!!!!!!{\"!!!>>}},{<!i>},{{<a'!!!>!<\"i!>,<>},<!>},<!<{!a!!!>},<{'!>},<{a!!!>,<>}},{{<i'!!!>!!ui}!!!>!>},<!!{!!!>a!<{{>},{},{<a!>},<}e'ea!>!!uo<!>>,{<\"{<u'o\"!{i<>}}}},{{{{{},{<}e\"!>,<!!!>},<>}}},{{<!!!!oa!o\"}!!!>ae!>,<!!>},{<!>io!!!>{!!!>e>}},{{<!!a{!>>}}},{{<'!!ie>,{<{!>},<e>}}},{{{<i<!>!!!><,}>,{<!!!>'}{!!\"!>,<!<!>},<!!!!!>,\"!!!>},<!>},<!>},<>}},{{},<e!!e!>{a!!!<>}},{{{{{{<>}},{<!>,<\"ue>}},<<!!u!{i!!!>!>o!>!!!!>},{{<!!!!!!!>\",!!!!!>},<!!}oa!>,<!!!>\"!!!!ao!!'>}},{{{{{<!}'!>,<!!!>!\"ae!!!!!!!e,>},{}},<!>aoai!!'!\"uo\"!!!>},<!>!!<!>,<>},{{{{{<!u!oeu,e!!'\"<u!!!>e'e<>,{<!!!>i\"ieu!!!>},<!!!>!!{>}},{},{{},{<i'!!!><a,uou}!>,<!>},<!>},<''!>,<u!!<\">}}},{{}},{}},{{<!>,<,!>},<!>},<<!!u<!!!>}>},{{<u>}}}},{{{<\"i{>},<>},<eo<{<!e!!'a!>,<,\"!>!>}!>,<ueoi>},{{<!>!>,<{e!>,<!>},<<ie'!!{!a<!e\"!>},<!!!>>},<!>},<o{!!!iu!!i!!u<>}},{{<e!!!>{!!!>o<!!u!!i!>}e!>},<a'>}}},{<<!>,<u<<!>,<'!!'ae!>},<!>{!!,>}}}},{{{},<!!\"'!>},<\"!>,<>},{{}},{<!>},<!}!!!>!>},<!u!\"e}u!!!><o'!!!>>}}}},{{{{<\"!!<!!'e>}}},{},{{{<!!i!!aa,>}},{{{{}},<e!!!!!>!!!>!!!!!>!i!>,<{!!!!!>,<u!>},<'u\",>},{}}},{{<<!>e!>},<a!!!>}}!>,<auii!!!!\">,<!>},<'\">},{{<{!!!>!!!!!>!!{i\"!!!!!!!>!!!>!>},<i!!!>},<!!!>,<!>>},{}}}},{{{{{<{!>,<<!!i!u!>,<!{oi\",'a!>>},<!>},<'a,!>!e'u!!o!><}ii>}},{<!>},<!!!>>}},{},{{<!!!>!>},<i!!!>},<>,{<>}},{}}},{{{{{{<!ia!>,<}!!!>>,<\"!>},<{!!}!!}',!!!>},<'!!>},{{<ii>},<!!!!!>u!>},<!>,<!!a>}},{{<uo'!<!u<!\"!>,<!>,'!!{ei'>},{{<!!!!{<!oo,!ooua{o>}}}},{{<!!}au!>,<!>},<!!\"!!!>!!!>!>},<}'\"\"{i!!!>!>,<>,<eoe!!uu!e\"uio\"i!!!>},<>},{{<e!!!!!}!!>},{<{{!>!>!!}i!>a!!!>>}}},{{{},{{<>},<!>,<!<{!>!{au!>},<!!ie>}},{{<!>},<\"!>ui>,{<<!!!>!>,<>}},{<\"{!>!!!oi!>'>}}}},{{{<\",\"!>u>}},{<!>},<{>}},{{<{!'<ii}}i!!,!,>},{{{<<o!\"{i\"!},ee!>,<!!!>'a!>},<>,<,ei\"!!<<u!>!!!!>}},{{{<u!!!>\",!!!>}!!a{>},{{{{<!!ou\"{e<{}!>u,ue!!a<!>,<<!\"!a>}},{}},{{<!!o{a!>>}},{{},{{<e,\"}{!>,<!>,<!>e!!>,<o<!u!!!>ii!'>}}}},{{{{<e!!!>!!oe!>},<}}!a!!}{!!{>}}},<!!,o!!\"!!<>}},{{{<!!{'!}o>}},{<!!!>,<!>!>e!>},<!>!!!>>}}},{{{{<!!!!!>!!!>>}},{<eo!!!>,o!>,<!!\"!!!>!'{ie!!!>u>,{{{{<!!,!>,<{<!>,<''>}}},{{<!!!!!>},<{!>},<u{}oi!a>},{<}!>!!!ea}!!a,!>,<!>!}!>,<!o}>}}}},{{{<o}!>>}}}},{{<<!!,!!,!>},<!>,<eu!>,<!!!>''i!!!>>},{<\"!!!>,<oi'\",i,e!ou>}},{{<<'au!!!!i!>},<,!!!!!!e>},{{<i!>,<e}>},<!!}!!!>}}!!,!!!!!!<!!!!!>\"!!!>!!e!!!!!!!>o!!!!!>!!>}}}},{<,!>,<!>\"\"\"u'!!\">,{<!!!>!>},<i!!!!!\",oeio!>,<!!<\",'o!!!>!!>}}},{{},{{<'!>},<e<,ou}>},{{{{<!!!>!o!>,<!!o'a!!!ee!!o!>},<u'!!!<!e!!,}!!a>}},{{{{<!!!!!>!!!>!>},<{{,}i>},<!>},<{u!>},<{!,aeu>},<i!!<!eu,u!!!!!>!!!>!}!>o<!>,!!!!!>!!>}}}},{{<u!!o{!!<ou{>}}}}},{{},{<!>!!!!!>!>!>i{<'i!'iei!<}!'<!>>,<u!>,<!!uoaui,!\"\"i,\"'{u,!!,>},{<!>,<!u,a!>,<!>!<!!u!!'!>,<}!>},<'i'!>,<i<>,{<o!!!>ai\"!!o<e!!!!!>},<{a!!<{,<'o!>},<>}}},{{{<}!!!>,<>},<a{!!i!i!!!!<<o!}e!!!{>},{{<}!>,<!!!>'!!!>ia!}{>},{{},<u{a!!!>!!a!!!>>}}},{{{},{},{}}}}}},{},{{{{{<>,{<{!>,<,}ea>}},{<{'!>\"i!>},<{<!!!>!>,<!!!!!!{o!!!!!!i!>,<!>,<>,<!!!!!>},<!>!i,!!!!}{'}!!!!u>},{<}{!>},<,i!!o}!!{!!!!u'oo!!e!>,<!!!>},<\"!ai>,{<a\"!!!>},<i!>!!!>},<e!<u,uo>}}},{{{<'ue>},{{<o!>,<!>,<}\"ia!!u{!!!!'>},{{<>},<,}!!,u!!!>>}}},{{{<!!!>i!>,<!!\"o!!\"o!!!>!!!>,>},<!!!>!!{!!!>,<'!a!\"uo!>},<!u!!!!!>'<'!!,>},{{{{}},{<a!e!\"}<!!'a''\"a!!,>}},{{},{{<a!e''!>},<o!!'i!>,}!>},<!!!!!>>}}},{<a!!\",!!!!!!!!!!oe,e>}},{<!>o>}},{{<o}'\",!<a!>},<!!<!!!>!!>,{<oe!{a!>}!>!\"!!e!!{!>},<o!!i>}},{<!>,<!a\"!>,<}!>,<!!!>!!!>,<>}}},{{{<!!!!{o!>>},{<o!!!>uauiu!>,<!!!>!,i!>,<}>}},{<!!!!!>!ou!!,!!!!!>,o!>},<>}},{{{{<'!>,<a!>>,<!>,<'e!<!!!>u!>,<!!'{!>\">}},{{{{{<e,!!!!i!!!>,<!>},<,e!>},<>}},{<,>},{{}}}},{{{<!>,<>},{<!!u}i<{!>},<>,<!!!>uo<o!!\"o<!\",!!!>!>,<!!!!'>}},{<{!>,<!!<ioa!!!>i\"!,e!>},<}>,{<o'{!>},<}u!>},<aaa{'!!e>}}},{{{<oa!!!!!>!!!!<!>,<!!'!>a>,{{<!>'e>}}}},{{{{<!!{e!}!!,!>,<!>},<!!!>!!ie>}},<',\",>},<{\"!}{!!!!!o>}},{{{{<!>!}!>,<!>,<!!!!!!oui>}}},{{{{{{<'!!!!'!i!}\"!!!!!!!>!>},<,<>},<!!!>!!\"!>},<>},<'!>{o,!>,<>},{<!>e\"!>,<\"}'!!!>!>',>,{}},{}},{{<!{\"!\"}a!<>},{{<e!>},<<!>},<!>,!!!!uo'<!!{o!>},<!ou>},<>},{{{},{{<!e>}}},{{<!>!!!>e!!,!e'>},{<i},!>},<uo!!!>!!a{a!>},<!!!>},<a!>},<{u>}},{}}},{{<>,<'i!!i,!!e!!!!!>},<<}u>},{<<!!!>,<!!''ui!>},<>},{<!>!!!>u!>,<!>,<!!!!,!>!!!!!}>}},{{{<>}}}},{{{{<'!>,<\"a!>e<!!a\"!!!>!!!>,o!>},<,a>},<!!!>,<!>,<e'!!!!!>i{e!>,<a!>,<!>},<a,>},{<,!\"e>}},{<}{>,{{<!>ao\"!>},<,>,{<o>}},{}}},{}}},{{<'!!i!>!!i!>},<!>},<!e!>},<!>!>,<!a!o!!!>!io!!!>>,{<!>!!!>!>!e!'\"\"!>!>,<}eu!!<!>,<'!>},<,>}},{<!!'u<ei<ii!!!>!>}>,{}},{{<,\"!>!>},<e!!i!>},<{uo>},<!!i{!i!>},<!!<}a{!!!>},<!!<i}au>}}}},{{{{{<{!!!>i<e!!}'!>a>}},{{{<,\"!!!!\"!><e!!a!>o>}}},{{<!o>}}}},{{<{eee}o>},{<!!\"i'!>},<!!!>},<'!!u,!!!!'o!!!><!>,<>}},{{{<!!!>!>e!>},<'}i{'u>},<e\">},{{{<!!o>,<}{!>\"!>,<u<!>!!\"!>!>},<>},{{<!!!!e>,{}},{<!!!!\"!{i!!!>!!<!!!>,<<a!>,<!!'i!>},<i>,<e!>!!!!!>!!!,\"!!!!\"!!,!!{!>>}},{{{<o{,<\"!!!!u>},{}}}},{{{<uo>}},<!>,<!!!>!>,<a>}},{{<!i!!ouue!!>}}}},{{},{<>}}},{{{<<!!<'a!!!!!!!>ae!!iia>},{<!>\",!>!!!>!>,<{e{!><!>!oa!!o!>},<<,!!!>,<>,{}}},{}},{{{},{<!!o!!!>{eu'!>},<o!>,<\"eo>},{{{<!>,<!!ii'>}},{<a>}}},{{},{<!>,<e!>},<!!<o!>}!{!>,'>,<!>'}a!!!!,e,{,e>}}}}},{{{{<'u!!!a!!}!>a!>},<!!iuo!!!>!>},<!>,<!!!>},<>}},{<ao!!!><!!!>i\"'{!!!>!!!\"u>},{{},{<!>,<\"}!!e\"!{oo!>},<}<!'}!aiu!>!\">}}},{{<e!>},<}!!i!!'<>,{{<{!!\"}{<i}>},{<!!!>,<ooi\"}e!\"!!'ei!!!>>}}}}},{{{{{{},<o\"o'!<!>,<!iii}o!iai!>u!!<'!!!!>},{{{}},{{<!!!!ueu!!!!!!!>!o!!!>!>},<o'u>}}},{{<!!!><!!!!!!!>\"!>e{i>,<'}!!!>'ao{}!!!!!>>},{<!!u!!!>!!!>,<ai>}}},{{<!>,<!>!>},<a}!!!><!!!>},<!!!>i!!!>\"<>,{}}},{{<aeu!!!>!!!>}!>},<!!!!!>{!!!>>,<!>},<!!!!!!!>,<}!!!>,<!!u!!o\"ui!u!!e>},{<!!!>!>!!!>,!!}<!!e{,>,<>}}},{{},<!>!>},<!!!>},<<a\"!>,<!>,<!>},<>},{{}}},{},{{}},{}}}},{{{{{<i!>!>,<!ueoea!!i!u}o}!'\"{>},{{<e!>,<u!!,!!!!\"!>},<!>},<{>,<ii!>,<'!!'oo\"!>,<a\"!i{>},{{<'u<!>,<aa\"o!!,!>}<!>,<!!!>ee>},{}}},{{{<!!!!!!!>!!!!!>,<!!<!>,<!>!}{!>},<!!!!!>>}},{}}},{{{<i}!!a'u,}!>,<'e\"!>},<>},{{<<a!!u!!,,!>,<e<>,{<\"<uo\"!>>}},{{<ie!>o,'!>,<'<e!!ia!!!>!>},<!!!',!a>}}}}},{{<<a<e}!'u\">,<!!!>!<oao!>!!aio!>,<!>,<u>},{{{<<!>,<!>,<\"!>},<e!>!!i<>},{{{<u\"a!!!>,<e!!!><!>!!}!>},<!!\"ou!>},<>},<!!!>!!!>!!!>,<<,}>},<!!!>!!!>ia!>},<,!!'\"!!!>!'o}u\"!>>}}},{{<>},{{<}!!!>>}},{{},{}}}},{{{},{{}}}}},{{<<!!,u'>,{}},{{}},{}},{{{{}},{},{{{{{<,>},<!,!!!!,!>},<{!!ii!!!{!!e!>!!!>i!!!!!>>}}},{<!!!>!!!>!!e!>u!>},<!!!>'}!><oa{>,{<e!>},<!\"!>o!>},<'<,!>!!<{e'\"!!o!!<>}}}},{<!><u!!!>\"\"e!{u{,,!!>},{<ee!!{,>,{<!io!>,<!u<}\"\"<!!>,<{!!!>\"!!!>ui!>}!!!!i,a!>,<!>,<i<'!!!!>}}}},{{{{<}>},<o!>,<!!!>ue!}<{>},{{{{}}},{<a}!e!>,<!>!><i!>},<!>,<!>,<>}},{<!>},<,{!!!!!>o<'\"u!!!>,<,!!!>!!'>,{<o!!!}ui!!!>!!{\"{!!oa!!i{!>},<,>}}},{{{},{<!>},<>}}},{{{<!!,}!ue>},{}},{{},{{}}}},{{{<,e{!!!>o}}i!u!>},<!>}ue{u!,}>}},{<u!!!>!>e,!!o!!!!a'{u>},{{}}}},{{{{<\"!!!!!!uo{a!!'<!!!!!>},<{\"!!'!>,<i!>,<<>},<!>,<{\"e\",!!u{o!>},<'!>,<a\"!!a>},{{{},<!!!>a!!!>,<!>oa'!'!!e!>,<,!>},<\"e>},{{<!>!>i,>},{},{{<o{!>},e!u'ui{!>},<!!!ooe!!!>u<>}}}},{{{<!\">}}}},{{{},{<e>}},{}}}},{{{{{{{{}},{{{<{<}>}},{{<i!<>}},{{<!!!{,u!!!>!!o>},{<a!{,!!\"\"e!!u}!>},<!!!!!>!>,<!!!>,<\"!!!!a!>,<>}}}},{{<!!aia!!!!ii!!i<!'!>,<!!i}a>,<\">},{{<!!!u}!!u!>,<!!{{!a>},{{<!!!>!>,<,>},<!>},<!>{>}}},{{{<u}!ue{'>},<!!!>ae!>,<!>'u!>},<}!!'u,!}!>,<!\">},{{{{},{{{},<!>!\"}!>,<!!!oi\"!u!!e,>},{}}},{<'u!}!a!>,<!>,<{{!!!>{>}},{}}}},{{<\"a!!!!!>,<!!\"i!!!!,\"!!<!!!!i,,i\"!>{!>},<!>>},{<!>},<!>},<i{u!>!!>},{{{{<!!u\"i!>,<u!>!!!>'\"!>,<a<\"a!>!>o!!!!!>>},{{}}},{}},{{<!!u{o!><,!!!>i!!{e{>,{{<!!!>,u!!!!'!!ueua}!>,<!>e{\"i!!>}}},<!>},<!>,<'>},{}}},{{},{<!!!>'u!>,<{o!!a!,e}'!>},<i{u!>!>},<{>},{{<!>!>!iou{i!!<!!a>,{<,{!!!>,<{!>},<!!!>!>\"{!u>}}}},{{{{<!>,<{eo!u<i\"!!!>}ii{!ae'!!!>>,{}},{{<!!e!>},<!>u!>,<!!!>!!!>},<>}},{<!!!>u!>},<ie>,{<!>,<!!e!>},<>}}}}}},{{{<>,<!!<{!!,e\"e\"e!>!>},<!>,<!>},<!!!>i<a!!}>},{{<!!!>,'',o\",>}}}},{{{<,!!'eo!>},<!!!>u{i>,{<!>,<{<<!!!!o!!!!!>!>!!>}},{},{{{{{{<a!>},<u{a'!>},<!>,<>},{<!>},<!\",!!\">}},{<{u>}}},{<{\"!>,<<>}},{{<'a}>}},{<!!!!!>!>!!e!u!!!>!>,<!>a\"}!>e!>'{>}}},{{{<e!>,<<i!>e!!!!!>,<!!!>},<>},<'!!!!e''!>{o!\"!>,<u!!!>u}>},{{<\"!>!!!>!!o!>e!>!{!!ia!>,<!!>,<'<!!!<,!!!>!!!>!!!>\"}u!!!!u>},<o!{<!!!>{>},{<!>,<u!>!{!{,o!!!><!\"!!!>},<!!e>,<!!!>!>},<}au,!!>}},{{{},{{<}!>,<!!!!!>!!o!!,>},{<!!,!!!>,<<!>,<!!uea!!!!uue>}}},{}}},{{{{{{<'!!!><!!}>}},{{{{<a<a\"!!}>}},<o!>u!!!!!!!!!>!!a!><!!u!>},<>},{{<<o!!\"a!'!!!>,<uua>}},{{<!>!>},<!!!>!o!i\"\"}!!!>{>}}},{<!>,<e'!>},<!!!>!>!>},<!><!!!!>}}},{{{{<!>,<!>},<e'!!e!!!>oa!!!u\"<!>!{>},<\"<}<}a!!,>},{{{},{{<!>>,{}},<!>,<e>}},{{{<a\",<e'e<i{i>}}},{{{<!>},<!>a!>,<'!ou'!!!>i!!!>ai>},{{<<u<!'!!!>!!!>!!!>'{a!!u}!>,''!!!!>,<,\"'!!o\"}!!!><<>},<u!!,!>},<!oo!>{!>,<!ui!!!>>},{<!i!>,<i!>,<<{u<!>},<\"<,}!!!>!,!\">}}}}},{{{{},{}},{{{{}}}}},{},{{{{<!>e!!!>!!!!'!!!>e<>,{<!>,<}!!uuu!>e{,{ee{!!!>>,<!>,<!eu<'!!i!>!<!>>}}}},{{{{<'!>ui}\"!!!>!>,<\"<!>},<iu!u!!!>>},{}}}},{{{{<,{!>,<{!>u!!!>}aueu!>},<!>},<'{i>},{<a}!!!>,,a!!!>>}},{<!!u!!!!!>,<!u>,<}!!!>{{!>,<<e<a!!!>>}},{{{{<o<!!!>\">,{{<!>},<i!!<!!<!!!>\"!!<<e!!!>!>u>}}},{{<u!'!>},<!!ia!>},<a!>,{!\"<'{!!!>',>,{{{{<a',e!!!!!>!>},<i!>,<e<!!oei}{a!!!>},<>}},<!!!>,<{!>,<!\"!!'!}!>,<!>>},{<i<!!!>!>},<!!!>u!>!,\"<{!!{{i!!!>,<!!!>>}}},{{{}}},{{},{<!!e!!{!>!>!>},<!!o,}!u!!e>,{<}}o!!!!}!>},<u!!i!>},<}>}},{{<ai!>},<\"!}!>,<{,u},!>,<!!!>!>>},{<o>}}}},{{<!!!>,u}!!!>,<}a!},{i!>},<>}}},{{<!>\"o!!'!{!!e!>},<uo!!!>!!!<<o{!!oe>},{}},{<!>},<ie\"aaeoi!!!>>}},{{{<!!!!!>!!ai\"u!>!!!>i{i>,<ue'<o!>,<{i'!!aou,!<a'>}},{{<iu!>,<!!ii!!!<!>},<!!!>}!>'a!>,<!>!>,<>},{<!>,<<!{o!u!!ua<u!!i!!!>},<'!!a>}}},{{<o!!!!ea!{'\",>},{}}},{<o!!e!!!!!>!!!!!\"!!!>!>},<!!!>}a}<{o>,<!>},<!!o>}}},{{{}},{{<!}}e!!<!>eu}\"!>},<}i}iea!ei!!!>,<>}},{<!>,<!!e!>},<'!!!o!!{}!e\"!!''!!!'!!!>a>}}},{{{<!>},<!!!>e{oo!!!>ii!!!>},<!>!!!!!!ao{e'!>i>},{<ue,}'!>,<''!>!>o!>,<!!}}'<>}},{<<},!>,<!>}>,<a!>,!>},<!>,<!>,<!>>},{{{}},{<}!!\"a,!!!!!ea!!}iu'}>,<u!}{!,!!!!ee!!!!\"'!!ou{i!!!!!>\"a!>},<>}}}},{{<!a<u\"\"{!!!e>,<o!!i!>!!!>oa''>},{}}},{{},{{<}\"<<e!>,<u>}},{<oo!!,ue!!!!!>>}},{{{}}}}},{{{{}},{<,uaaou!!!>>}},{{}},{<!!!i!!!!!!<{'eo,}>}},{{{{<{'uau!>,<!!!!!>!'{!!'!>},<!>,<!\"u!!!>,<!}o>}},<,}u!>,<!>},oe<<<<!><!!!><>}},{{{<}!!!!!>o!!!>},<a!ea!>},<!>,<,!!!!!>\"o>}},{{<\"!!!>!!>},{<,!!!>o>}},{{{<!!!>!>!>,<!!,}eo{!!!>!>},<!>!>!!!>},<a>},{{<oe<,!>u!!<!>},<u{'e}{ie!!!>i>},<eu!!<\"!!!>!>'!>},<e!!u,!>,<\"oe'>}},{{<o!!,!>},<!!<>}}}}},{{{{{{<!!'o,}}{!{aa\">},{<o!!i,,>}},{{<{!>e!>},<{!ua!>ea,a>},{{{<<{<u!>oa<!>,<o>},<>},{<!!!>!oeo!e{!!a\"!>!>},<}!><,<ii>}},{{{}},{<aio!!!!!>!>,<u!!e!!!>!>},<\">},{{}}}},{{{}},{{{<u\"a!!o!!<,!>e!!!>!!!>{!!>},{},{{},{{<!!!>!!!>!>!!!>},<ou',>}}}},{{<!>},<'i'{ue}}>}}}},{{{{}},{<>,<!\"!>},<}ii!>,<!\"!>!!!>o!>},<<{!<>}}}}},{}},{{{<<e!>!!!>!,>,<}!i>}},{{<,!>},<a<o{a!!'!a!>!!!!!>,<!>,<>},{{{{<!!!!!>,<\"!!i>}}},{{{{<}!!!!!>'!!!>ia}!!!>!e!>,<!>,<<e}>}},{{<!!!>,<!!}<}a!>,<\"!!!>{e!!>},{<i!>!>},<>,{<<,!!o!>},<!'u!>!>,<!,!!<!!!!euaoo>}}},{{<!!!>!!!><!}io!>,<>},<!>e\"oau!}o!o!!ii}\",}}!>,<}>}},{{{<!>,<e,iuo<e!!!>!!}!>,<>}}},{{<'i'}!!!!\"!!i!,!!{a!}!!!>!o>,{<!>},<!eeu<,o!!<\"!!!>!!!>i,a!!!!!>i!!u!!,>}},<\"{>}},{<!>,<!!}!!\"<<a!!a!!u!>,<}>}},{<{}!>},<!>e>}},{{<<a!oo!>!>},<>,{<!!a!>,<!!!>!>'}ia>}},{{{<!a{,!>},<!{{>}},{<!!!!,aa!<>,<i!>!>,<i!}!ua>},{<!!!>}!!!i!!!>}!!!!<{!>,<',!>,<o!!io!>},<uu\">,{}}},{<o!!aa'o>,{}}},{{{{<{!},!>},<u!>!!!>\"i\"'e!>,<',{\"o>},{}},{{<}!!!>,,ia!!uo>}}},{{{{{<!!'o!>!!<!>,<!>,<!!!>!!'u{e>,<!>,<!!<\"!!!!,\"{!!o\"eo!><>},{{<>}}},{{<!}!>!>,<<!!!!!>!>{!i,!!!!!!\"ai!}{!>,<}ia>},<!!!!!>},<>}}},{<!!!!!!!!{\"!!'{u}>,<!>,<u!>,<}},!!!>>},{<}'!>\"!!!>>}},{{{{<>,{}},{},{<!>,<!><!>e\"eii!,e{<!!!>,<,>,{{}}}},{{<!!i}!!a!!!!!!oo,\"{o}a\">}},{}},{{{}},{<!>}!>,<ao!!!>,<{{>,{<>}},{}},{{{<!!!>},<u{>},{<eao!!,!>>}},{<'e!>},<!>i'u{!>,<}>}}},{{{<!!!>!!}!!!>i!!!!i,!>!>,<!!},uu{'i!>,<a!>},<>},{}},{{<!>i\"!{u!>!!!>,<>},<u!>,<iau!a!!!!!!a{!!!>a,o}\"o!>,<ui>}}}},{{{{<o!>,<a!>,<!u!>!>,<!>!!!>!>e!!au>},{<\"!!!>!!{eeiao'!!!>ae!!!>{!>,<!!!>a>}},{<!!'{eue'>},{<{\"<u}'!>,<,!!!>\"!>,<>,{<!\"a!!!>\"e'!>},<{!>,<\"!!!>o!!!\">}}},{{{{<,i!!!oae!!!>,<,!>!!o>,<!!!>!>!!a!>},<!!i>},{<!!}<!>oa!!i!>,<>,{}},{{<'a}e'e>,<a!>!!i!a,!a!a<>},{},{{{<!{a,!!!!!>!!!>u!!!>,<<!>},<i!>!!u!>,<!!!!>},{<a{'!o!>!!e!><!a>}},{<e!!!>!>},<!!e!!!>!>},<,!!!!'!}}o!>a!!!>\">,<!!!>a>},{}}}},{{}},{{<}!!a,\"!!oi!!!>!>,<\"!!!>,<!>,<i!!!>o!!!e'o!!!>!u>},{<!!'!e{!>,<a\"!!e!>,<u<!!}!<>}}},{{<!>,<oe!!!!!>,!>,<!!o'!!<!>,<>}},{{{{{<!>,!>},<!>!!!>},<!!}!!u,!>},<>},{}},{}},{{},{<!!!>aa',!o'!>},<}\"!!!!'!!!>>}}},{<>,{}},{{},{{<!!!!e'!>,<!>,<u!!!>'!>{e!!!>e}!>,<\"a>,<a\">},{<!!!>,!{{!!!!u!!ea<,{,{>,<aoi!>i\"i!!!>!>},<!>,<!>},<!>\"!!!>\"u!!\"\">},{{<<!!!>e!>},<!>,<\">},{{{<!!\"!>},<<<!!!!!>o!!!>,<}!>,<>},<e!>},<!!e!a!>},<,!!\"e!!!>!>},<!!!!!!oi!>>},{<!>},<i!!!!,o!!}<{}!>,<!!i,!>!!o!{>},{<ea!>!!!>{!i!!!>},<<>,{<o!>!!!>,,>}}},{{<}!>,<>,{}},{}}}}}}},{{{{{<!>,<u!!eiuu!i>}}},{<e'!a!\"!!!>!!!>},<oa!!!>},<{>,{{<!!{,!!!>!>,<<<e''}\"o!>,<a}!!'>},<!>},<,'!>,<{a\"e'<}!>o!>},<<\"\"!>,<>}}},{{<!>,<e!>,<a<!!\",,i!!!>!>,<aa>},{},{{<!!!>,<!!!!!>{!>,<!!!>,<!!io>},{<>}}},{},{{<!aee!>},<!!\"u!>>,{<!!!>},<iei>}},{{<\"!>}!>},<!{{o{e<!>},<!>,<!>>},{<!<!!!ou''>}}}}},{{{{},{},{{<,{!>{!!aii!>!>e,>,{<!!}>}},<}!>},<{>}},{{{{<!<!!{i!!!>},<>}},{{}},{{{{<\"!!!>!!!>o!<'ea'!!!>!>!>},<'>,{<!>\"<!!i}a}<,\"!!!>!{}}!>},<>}},<<!>},<i>},{}},{<o!!!!!>},<<!!>}}},{{{<!!!!{,!>},<!!!>u!!!i!!!>!!<>},{}},{{<u!}o!>!>},<'!u,\"!>},<>}}},{{{<}ae!>,<u'e!!!!'!!!>o!!!>'u!!u!!!>!!e>}},{{<!>,<!!!>o'{\"!ii'\"e}!>},<}>},<u,i!>!>},<!!\"}!>!>,<<{a>},{{<!>,<!!e!>},<>}}}},{{},{{<!>,<!!'!,!!o!!!a!!u>},<<!!<!>,<!!{!>!<a!{a<>},{{},{<}!>},<,{>}}},{{{<o!!!>!>},<!!<}u!>,<{iuu\"!!'}a,,>}},{<>,<!!!>,<}{!>!!!>!!}!!}!>,<!>,<o!!,!!!!!>>},{{<,!!!!!!!>},<o\"{!!!><{,{!>}!>,<!!!>!i>,{}},{{{{<!>},<e!!<}>},{{},{<\"a!>,<!>'<!!!!!>},<!!!>},<>}}}},{{{},{<,!!!>'!>},<},\"!!!>,<u!!,\"!!!!!>!>,<ee>}},<<!u<<},!!!>!>},<e'!!<o>}},{<ue}{!!}\"!>,<i{>}}}},{{{<!>!!!>!>,<a!>},<!>},<!>ou>},{{<ua'o!!{au{\"e!!!>!>,<<'>},{<o!!!>a!>},<a!\"!!e<'!i<,<!>},<ii!<!!!!!>>}}},{{}}},{{{<>}},{{}},{<o!>},<!>},<!>,<\"e!!!>}!>},<,,!!!>}!!!!\"!>},<>,<!>},<e!>},<\"!!e\"oa!!!>>}},{}}},{{{{{{{<o,}!>},<!>,<!!,{!!!{,o>}},<!!o!!a!>},<\"u!!!>},<'!!aea>},{<!u\"ooi!>,<u<'\"!>,<!!!>!>,<<!!!>!>,<>}},{{<ua!>!!e!>},<\"{!>o!{uaa<!!>},{}},{<\"}\"!!!>'!!!!\"'o}!!!>!>\">}},{{{<!!iu!',oi'!!!>oe>},<!>,<!!!!a'!,!>i{a{!!!!>},{{<!!!eu!!>},<!><}i!>},<,o\"!!a!>>}},{{{},{{{{}},{}},<!>},<iaa!!!>'o!!u!'>}},{<>,{<\"i!i!>ao!!!>!!!>\">}}},{{{{{<!>,<!>},<!>,<u!!oe!!!>!u<>},{{{{<!!!>!>,<e<{oa!\"{!>,<'!a,e\"ee!>,<<>}},{}},{<!i}eoe\"!!!>!!!!o''}i,!!!>!!a>,{<!{,!!!>},<<\"!!!},!}!!<}o\"!u!}'!!'>}},{}},{{},{},{{<{!>,<!\"'a!!!>!>>}}}},{<\"{!!!!e!euui''o!!u}!!!!u\">,{{{}}}},{{{{{<uo}>}},<e!oia}!i\"!>uau'e!!!>!>},<,u!!>},<}!!a!!!>u!!>},<ueoi'!}>}},{},{{{{{},{{}}}}},{{<i!!!>!!!>},<u<!!!>!!!>ie!!a>},{<<!!>,{<!!{!'\"}!>},<e!!!!e!!<!!!!!>},<!!'>,<}\"!!{!>,!!!>!}>}}},{<!!i!!'!!i'>,{}}}},{{{{{{<o!!!>>}},{{{}},{<!!!>'{u'i,ie\"o!!u!!,o>}}}},{{{<!>!!<>,<'>},<<{ua!>,<<<<!>>},{{{<>}},{}},{{<'!>},<!!!>,!!!!!><>,{<!>,<,a!!,<!!<}!>,<,!>,<oi,!!!><!!iu>}},<!'!>!>},<<ue!!!>,!}<\"{i!>!!!>,>}},{{<oe{!>u',}!>},<!>,<\"!oe<ai'>,{}}},{{<o!!!!oe!ou!!!>!>>,<!a'!>},<{,{{io>},{{<>},{}}}},{{{{<!>,<o{}!ouu!>},<!>},<!!!!!>,<!!>},{}},{<'>,{<!aa!!!!!>}<<}i!!o}!>,<!!!!!!!!!>,<!!{>}}},{{}},{{{{{<!!!>},<>},{{<o\"!!a>}}}},<!!!!e!>,<e,}!>!>},<}>},{<}{<!!ua,!!ei!,!>\"a>}}},{{{{{}}},{<<'!>!!!<i!>,<!!a!!!>{}!!!>!!!}>,{<,{!!{u,!!!>,!!!>!>!!!!i!>,<!!!>!>!>,<''>,<u!e\"i!!!>},<\"!!<uo<,!!!!!>,<!!!!>}},{}},{{{},<oa'!>,<!!!>a'!!!>,<!>,<>},{<i!!!>},<!!}!!i}!>!u,{{!>,<\">}},{{<>},<!!!>}!!!>!o},u>}}},{{},{<!!o,!>},<!!!>!>!!!>!!!>!!{e!!!i>,<}i!!!>\"!!,o\"!!\"\">},{<}'u!>},<i'!!!>>,{<>}}}}},{{{{{<{!!!>!>e{{>,{{<'i,i,,!!!>>},<uo!>,<o!>!!u}!!o!>},<{u{!!!>{>}},{},{{<!\"!!!!!>},<!>!<o>},{}}}}}}},{{{{{{<!!ei>},{{{<!!,u!!o!!o}}!!!''!!',!!!!!>,u>,{{{{}}}}},{<!!!!\"!!>}},{{<!>},<,!!o!!!>!>>},<!o!>},<!>>}},{{}}},{{{{<<{}<o'i!\">}}},<!!!!{<!>!>,<,!!}u{>},{}}},{{<e{aio!>},<,i\">},{{{{<\"\"iaia!!!>'!!\"e<!!o'!!'>}},{{{<!>!>!!!!'!>!!!o!>!>!u'!>,<!,\">}},<}\">}},{{<'u{!>a<i!>},<e!!!>o>}},{{{{{{<!>i,!>!<o!!{{!>o'<}'!>,<!}u!>,<>}},<!!!>!>},<!>,<!>!!!>!>},<'!!<}>},{<!!!!!{e<!>},<{i,!!!>},<,}!>u{}!!!>!!>}},{<{!!!!,>,{<!!!>!>},<!>,<!!!!!>},<!'{,}!>!>},<!!>}}}}},{{{<!!{>},<!>,<o!!!>>},{<io!>,<'!>o'i{>,<!!i!!!!!!!!,>}}}},{{{},<,\"!>},<!>,<{!!!!!>{!,,!>!!!>},<,}>},{{},<!!',\"!!!eo'o}'\"!>o>},{{{<!!i!!!a{!!!>},<}!>,<!>oua!!aea!e!>ai>}}}}}},{{{{{{<!\"!>,<!!\"i{!!!!!!}<i\"'>}},{{{<!!\"!o>}}}},{},{{{},<eo!>,<u\"!!o'!,!u\"!>},<!!!!{!!,!!!!!>!!!>io!!<>},{{{<!!!>},<!>},<!>,<!!!>!!!!ooaa>},<'!>!!!>!>!!!>u<eiou!!ou!!'!!i!!>},{<!!!>>,{<!\"\"!>a'{!!!<ie!!!!!>{<!>!>},<!!!>>}}}},{{{{<}u,{!{!!!>,i!>,<!>,<<u!ouo!>},<!>}{>},<,>},{{<!><}>}},{<ie\"<!>'!>,<\"o>}},{<}\"ia{!\",\"!>},<!!!>o!>,<!ii!!!>}!u!!!>>,{<{!!u>,{{<!!i!i>}}}}}},{{},{<,i>,<i\"{o!>,<\"!>},<u,!\"!ui!\"a>},{{<u!>,<!}{'\"'!<o!,!!!!e'o!,>},{}}},{{{<!!!>!>,<!!}!!{!!u'},'!!{ai!!!>!!{!>,<{}>,<i!>},<!!u}!'}\"!>},<u,!!!>'{!>,<>}},{{<<!>,<o!!o!!!!a{!>,<u}{!>e\"e!,!!ea\">}},{<ee}{!!!!!>\"!!oi!>,<!>},<{a}o!>,<\"<,'>}}},{{{{{{}}},{<}\"!!u}!!!><{<!>{i!>},<!>,<u!u!!>,{<a}!!!>a!>},<{<!{ae!!>}}},{{{},{{<!>},<!>,<,!><ue}!!>},{<!!>}}},{<!!!u\"i,'!!}'aa\"!>{!>},<{!>{!!>},{{<o!>,<i!!!!!>>}}},{{},{{<i!>,<!>},<!,{'>},{<!>i!!!,!!!>'{!!{!!!!{!!>}}},{{<!>!>},<u!!!>},<}!!{,e!!!>!!!!oe!>},<>,{<!>>}},{{<i!!o}ee'!>},<!!e!,uu,}u!!!>!>!>,<!!>}},{<!!e!>,<u>,<!>},<>}}},{{{<,!!!!o!>},<>},{{<!>,<}!!>}},{{{{<,<!>!!,e,,!}'>}},<i!!!>iu!>oa<!>,<!!!>},<!,!!!>!>,<<!>!!!>!!>}}},{},{{{<}!!!!!>},<!>,<ui!>>,<<!>,<a!!{!>},<!!<!>{}!!i'>},<!>,<}o{!!i}!!!!!>>},<e!!!!!>,<<>}}},{{{{{{<!>,<}a<!>i!!!>'>}},{<!>>}},{{<a!o{}i<{!!!>!>},<i!>},<i}!>!a\">}},{<!!!>i!>\"u}!!!!!>>,<<{!>,<!!!>},<<'\"!!!u!!,!!!ae<!>!>,{'>}},{{{<!!!>}!!!>!,\"}>}},{{},{<''{>}},{{<e!'!>o!!{!>,<i!!!>,<{'!!\"!>!!!>>},<,e\"u!!!>ao!!''>}},{{{{},{}},{{},{}}},{<oe!>!!ou!!!!'>,{<ooeu<}!!!!!>!!\"!>i!!,!>,<a>}}},{{<i!}!>},<i!!!>o!><>,{{<ee!!!>>}}},{<'o'!!au!>,<!>,!!{i<{!!!u!!!>{e>,{}}}},{{{{{},{<eiu!!e>}},{<a}},!>i!>,<!!<u!!!>,,!>!!!'!!u!!>,{<!!!!!!>}},{{{},{<{!!}}!>,<!!'!!}>}}}},{{{<u,<>},<!}<!!!>!!!><{!!!!!!!>u>},{{<'}!>,<i!>},<o{!>,<<u<<!!a,e,>},{}}},{{{<ue>,{<>}},{{{<!>,<!{ou'!>au\"e\"ee\">}}}},{{<a,u!!!>!''>},{<!eaaei!>!>u>}}},{{{{<!>!>},<eo}!>!>!>!>},<!>},<>}},{<u!!!!\"i!!!u!>},<o!>,<!!!>!>},<e!>},<!!a,o!!!!!>>,<!!<a!!!>a>},{{<,}u!!!>!!<!!!!!><>,{<!!!!i'<!>},<!>},<>}}}}}},{{{{{<\"!a!>},<!>!>e!>,<!!!>a'!>},<!>!!!>!>\"{>,<!>,<!>u!!a!!!!\"}!!!>>},{{}},{{{}},<!!e!!!>!!!!!!'u!>,<!!!>>}},{{{{<!!!>,<{e!!!>,<!>},<!!!!!>{!!a!>,<}e!>},<!>},<,!>!!>},{<i\"}a!!!>\"!>,<\"!a>}},{{},<e!<!<oi<!!'!>,<<a!!!!!>},<!>\"!!!!!{>}},{<!!,!\"i>,{<\"uo!>!o\"{{!!!>>,{}}},{{<!>},<!!!>}\"<{<aiaa!!!>i!>,<,u>}}}},{{{},{}},{<'<!<}o!!\"!!!><oa,>,{<,}!!!><a,!!!!u!!!>}!>,<!!!>\"oa!>,<<!>}!>,<>}}},{{<,!!!<o}a!!!>,<'{{!>,<,,<eu!!>}}},{{{{{{{<!!!!!!!i!>,<!!{!>,<eu!'!!<!!!>!!!}!<i!>>}},<!aie}!!!<!>!>,<!!}{,<!>!!'u<,!!!>>},<o!{!>,<!>!iae!!!!!>,!!!>,<i!!u{,ii'!!!>,<>},<\"\">},{<aiae\"'!>},<!!eo!\">,{<{o!>,<,{,!!!>!>,<!!!>}>}}},{{{{{<u!>},<!u!{\"'>},{{<!!''}!>},<<!>!>'\"!!iu!>,<!!!!e!!<>},<{!!!>!!a>}},{<!!',uo!!!ue,{!>},<ea!>o!>},<!>,<u>,{<<!\",>,{<'ei!!!!!>aii!>}!!!a!>},<a!<!>!!!>>,{<!!!,o<au<ouo!!>}}}}}},{{{<a\">},{{<oui!>}}>},<i!!u\"i}e<,!}i!!!>,!!ue!!!>},<!{u>},{{{}},{}}},{<e!!{e!!!>u!!!!,<!>},<>,<eo}<<a!>,,a!>},<!!>},{}},{{{{},{{}}}},{{<e{!!!>!>,<\">}}}},{{{{{}},{<\"'{!>,<>,{}},{{<e!!!!!>,!!{>,{}}}},{{<\"!!o!!!>,!>},<!!o!>!!!!!>!!!>!!{o!!\"!!a!!!>>},{<!>aa!>,<!>!\"!>,<>}}},{{<i!!<!!!>},<a!!i!>>},{{<,<<,,u{a!>!>,<!'!!,}!!!>{!!!!!>>}}},{{{<!>,<<>,<!ae!!e!}}u!<{,}o}u!!!>a>},{<aue!>},<!!!>!!!>a!>},<!uo!io!!!!<<<}!!u>}},{}}}},{{{{{},{}},{{},{{<!>},<!>'e<!!}>},{<!!o!!!!'!\"'!>},<>,{<!!<aao!!!>!!}>}}}}},{{{{<!!!><!!!>u{}ao\"'u!!!>,<!\">}}},{}},{{},{<!>},<>,{<!'eo!!<}!!!>!!!i<'oe\"!!!>!!!>},<!>,<!!!>,<{>}}},{{{<i\",>,<,ai\"\"a!!!!\"<\"a!>,<>},{{{<!!!,!,>}}}},{{{{{<!>,<!!\"!!i{'e!!,,>},{{<u}!!uo!>,<!!!>i>}}}},{}},{<u!!!!a\"\"!!!>io!!!>,}!>},<{u!!!>!!<!>uu!!o>,{<!!!>!>,<,{}>}},{<i'{'\"!!!>!>,<u!!{!!!>,<!>},<!!'eu!!!>},<>,{{<'}!>,<!!!!!>!!!>e'e!!a}\",!!!>!!!>!>\">},{{{{}}},{}}}}},{{<e!>,<}oa!>>},{{{<u>}}},{<}!!!>u<{!>},<!!\"!>a!\"'\"{!!e,}!!!!ie>,{{{}},{{<}!!,{ae!!uo{u!''!!\"!>,<e!>},<!!,>},<,!!!>}!\"\"!!!>!e{!!e!>,}u>}}}},{}}},{{{<!>'\">}},{{<ee>},{<!!!>!>,<!!!!<e!!!>u!!!>!>,<ai!>!>},<,'>}},{{<!>},<!>,<!>!>}!!oo!!e>},<\"u>}},{{<{!'\",i{u!!\"!>,<,'i!!!!!>'!>,<>,<>},{{<<uu!'!!!!!>i!,>},{}}},{{{<e>,{{<<{!>},<\"!!!>,!>,<u>}}}},{{{{<!>,<!>,<!ua>}},{<>,<!>!>},<!!,!!!>!>,<}!!!>!!,!\"\"\"!\"!>,<!}!!<ui>}}},{{{},{<o!!!>!>},<>}},{{},{<!>},<au!>},<!!i!!e'u}!!e\",>}}},{{<a!>,<}!!!>a\"a}!>!!!>!!!!!>,{!>,<,<e}>,<!!!!<i\"!>,<!\"u<!>,<!!,o>},{<o!>},<!>!!'!!u,>,{<'ooooouue!!!!},>}},{{<!>,<<!>,<!>,<{,'a!!'!><!!!!!>!!!!\"<!!!!!}e>},<<<!!}'>}}}},{{{{},{}},{{},<\"\"{u{!!!!}!!!>au!!,!>},<u!!aae!!u>},{{<o!!!e!>},<'!>,<e<u!\"!!!>!!!!u!>},<>}}},{{{},{<,{u!!}a!>i!>,<,!!i!>},<!>,<>,{<,!!aau<!a>}}},{{{<e,!><e!!oa!'!!'\"!>!!!>!'!!eeo}iu>},{{},{<'!\"\"a\"!!{!!!>!a>}}},{<o!>,<u'>,<!<!!a!!!>!>},<!i!!eu<o!!!!!>},<u!!!>{,!>},<!>},<<>}}}}},{{{},{{<o!>},<}e'u,\">},{}},{<u,!>!e!!u!!{!>!!!>!!},,{!<,<}'>}},{{{{{<u,ui}!!e!!!>!!!>\">},<\"!>},<'!>!>},<'{!u'u>},{}},{{}}},{{{},{{<,!!!>!>,<ei{!>\"!>,<{'!>,<u<a>},{<}e<!!!>},<'u,}!}!!>}}},{{<{!!',o<o!!!>},<o!!!!!>o!!!>u!>},<>},{<{>}},{{{<!'{,<'{iaeu!!{!>>}},{<e!\",'>}}},{{{},<<!eo'!>},<iu,o,eo!'\"u!!!>!>'!>,<>},{{{<'!!!>!>},<,!i!>},<!>,<e'>},{<!!!>!!<\",<,i<\"!!!>i'!>},<}i'}!>},<>}},{{<,!!!>>},{{<ia,!!}},a>}}},{{},{{{<>,<ee!!>},{<!!!>'eeoau!!!>},<iu>}}}}},{}},{{{{{{},{{<{!!!>'!>!!a>}}},<>},{{<'!>!,\"!>},<!>},<<>},{}},{<eu!!!>!>,<,\"!!'<}!!,!>},<'!}!!!!!!!!{<}>,{<\"o',,<e>}}}},{{{},{<{e,!'oi!>},<!!!>},<,a\"!!!!'}>}},{{<'u>},{<o!e!!!!!>{{!>,<!!ii,>}}},{{{},{{{<!<}oe}!!>}},<!>},<\"o!!!!!>{o!!!!oe!!{ua'!!!oe,o>}},{{<a!>},<\"'i<<!>},<}!!!!e>},{{}}},{{<!!i!'u,!>},<>},<u!!!!\"!ea<oe!!<,<>}}}},{{{}},{<<>},{<}},e{'u>,<{{i<o{a>}},{{<}!>},<>},{{<!>,<{i>}},{{}}}}},{{{{<{>},{<'!>},<}!>>}},{<!!>,{{<>}}}},{},{{{<}!!,i!!!>a!>,<!>,<>,<!<\"a'!!!>>},{{{{}},{<e\"uo<!,oe>}},{<e\">}},{<{\"!\"!>}u{!!<e>,<!>,<!!<'!!a!>,<!>},<,!>},<!!!>>}},{{{},<!!!>o!>},<ui>}},{{{},{}},{{<a!au>}}}}}},{{{{{<uo!!!>,<,,\",\"<a<!>,>},{<,ieo>,{{<!!!>},<,'>}}}}},{{},{{<!>!>e{,}!!ie>},{{{{<!>!>,<e,!!!!!!}!>ai>},<!>,<!i!!!!!>},<>}}}},{{<ue<!!i{!>,<i}i!>!!!>!>!}>}}},{{{<!!!>>},{<\"e<{!!<,','!a!!}!!}!>,<!<!!,>}},{{{<!>,u!>},<e}}!>,<!,{>},<!>,<<{!>},<aaae!>,<>},{<<'!>},<e'u>}},{<!>{a!>,<!>,<!}>}}},{{{<!>,<e'>},{{<'!>,<!a!>,<!!au!!o!>,<u!!!>!>!>>}}},{{{<!>},<!!io!>!>!>i!o!\"!>},<a>}},<ui<!!!>o!>,<i,o>},{{<!>},<uea!>},<{e!!<ie'>}}},{{{{{{{{{<!>},<a!!!>}o'!!'!!a!!!>,<>},{}}},{<!,,!!<a!>eu>}},{}},{<o!!>}}},{<u!!!>>,<<!>\"!!!>,<'!!eae!{!!{o!!,\"a{>},{}},{},{}}}}}")
