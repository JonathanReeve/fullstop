-- | Strings whose internal "." should not be treated as sentence boundaries

module NLP.FullStop.Ignore where

titles :: [String]
titles =
    [ "Adj."
    , "Adm."
    , "Adv."
    , "Asst."
    , "Bart."
    , "Bldg."
    , "Brig."
    , "Bros."
    , "Capt."
    , "Cmdr."
    , "Col."
    , "Comdr."
    , "Con."
    , "Cpl."
    , "DR."
    , "Dr."
    , "Ens."
    , "Gen."
    , "Gov."
    , "Hon."
    , "Hosp."
    , "Insp."
    , "Lt."
    , "MM."
    , "MR."
    , "MRS."
    , "MS."
    , "Maj."
    , "Messrs."
    , "Mlle."
    , "Mme."
    , "Mr."
    , "Mrs."
    , "Ms."
    , "Msgr."
    , "Op."
    , "Ord."
    , "Pfc."
    , "Ph."
    , "Prof."
    , "Pvt."
    , "Rep."
    , "Reps."
    , "Rev."
    , "Sen."
    , "Sens."
    , "Sfc."
    , "Sgt."
    , "Sr."
    , "St."
    , "Supt."
    ]

abbreviations :: [String]
abbreviations =
    [ "cf."
    , "eg."
    , "ie."
    , "i.e."
    , "e.g."
    , "v."
    , "vs."
    ]

initials :: [String]
initials = map (: ".") ['A'..'Z']
