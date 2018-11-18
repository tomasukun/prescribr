#' Library Class to hold exlusion criteria, study population numbers, and other descriptive data
#' @import dplyr
#' @import stringr

study_pop_library <- R6::R6Class(
  'study_pop_library',

  public = list(
    source_file_dir    = source_dir(),
    processed_file_dir = processed_dir(),
    shared_docs_dir    = shared_dir(),

    doc_specialty_categories = function() {
      suppressMessages(readr::read_csv('spec/partd_doc_specialty_tabulation_2014.csv'))
    },

    study_pop = list(
      study_2013 = list(
        partd_docs = NULL,
        openpay_docs = NULL,
        partd_specialties_keep = NULL,
        partd_us_docs = NULL,
        valid_brand_docs = NULL,
        phys_comp_docs = NULL,
        unq_match_crit_partd_docs = NULL,
        med_grad_exclude_docs = NULL,
        unq_match_crit_op_docs = NULL,
        final_study_docs = NULL
      ),
      study_2014 = list(
        partd_docs = NULL,
        openpay_docs = NULL,
        partd_specialties_keep = NULL,
        partd_us_docs = NULL,
        valid_brand_docs = NULL,
        phys_comp_docs = NULL,
        unq_match_crit_partd_docs = NULL,
        med_grad_exclude_docs = NULL,
        unq_match_crit_op_docs = NULL,
        final_study_docs = NULL
      )
    ),

    study_group_pop = list(
      statins_2013 = list(
        claim_count = NULL,
        meal_payment = NULL,
        tagged_payment = NULL
      ),
      statins_2014 = list(
        claim_count = NULL,
        meal_payment = NULL,
        tagged_payment = NULL
      )
    ),

    exclusion_criteria = list(
      doc = 'M.?D.?|D.?O.?',
      claim_count = 100,
      states = c(state.abb, 'DC'),
      grad_year_limits = c(1910, 2013)
    ),

    analysis_vars = list(
      doc_spec = list(
        'Family Medicine'   = c('Family Practice', 'Family Medicine', 'General Practice'),
        'OBGYN'             = c('Obstetrics & Gynecology', 'Obstetrics/Gynecology'),
        'Internal Medicine' = c('Internal Medicine', 'Hospitalist'),
        'Pain Management'   = c('Interventional Pain Management', 'Pain Management'),
        'Ophthalmology'     = 'Ophthalmology'
      ),
      region = list(
        "Northeast"     = c("CT", "ME", "MA", "NH", "RI", "VT", "NY", "NJ", "PA"),
        "Mountain West" = c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM"),
        "Midwest"       = c("IN", "IL", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
        "South"         = c("DE", "MD", "WV", "DC", "VA", "NC", "SC", "GA", "FL", "MS" , "AL", "TN", "KY", "TX", "OK", "AR", "LA"),
        "Pacific West"  = c("CA", "OR", "WA", "AK", "HI")
      )
    ),

    study_drugs = list(
      statins = stringr::str_c('LIVALO', 'CRESTOR', 'LIPITOR', 'ZOCOR',
                               'LESCOL', 'LESCOL XL', 'ALTOPREV', 'MEVACOR',
                               'PRAVACHOL', 'ATORVASTATIN CALCIUM', 'FLUVASTATIN SODIUM',
                               'LOVASTATIN', 'PRAVASTATIN SODIUM', 'SIMVASTATIN', sep = '|'),
      anti_mrsa = stringr::str_c('CLEOCIN HCL', 'CLINDAMYCIN HCL', 'CLEOCIN PALMITATE',
                                 'CLINDAMYCIN PALMITATE HCL', 'CLINDAMYCIN PEDIATRIC', 'BACTRIM',
                                 'SULFAMETHOXAZOLE-TRIMETHOPRIM', 'BACTRIM DS', 'SULFATRIM',
                                 'ZYVOX', 'SIVEXTRO', 'VIBRAMYCIN', 'DOXY 100', 'DORYX', 'DOXYCYCLINE HYCLATE',
                                 'MORGIDOX', 'ORACEA', 'DOXYCYCLINE MONOHYDRATE', 'MINOCIN', 'SOLODYN',
                                 'MINOCYCLINE HCL', sep = '|'),
      opthalmic_corticosteroid = stringr::str_c("MAXIDEX", "DEXAMETHASONE SODIUM PHOSPHATE",
                                                "DUREZOL", "FLUOROMETHOLONE", "FML FORTE", "FML",
                                                "FML S.O.P.", "FLAREX", "LOTEMAX", "PRED FORTE",
                                                "PRED MILD", "PREDNISOLONE ACETATE", "OMNIPRED",
                                                "PREDNISOLONE SODIUM PHOSPHATE", "VEXOL", sep = '|'),
      osteoporosis = stringr::str_c('ALENDRONATE SODIUM', 'BINOSTO', 'FOSAMAX', 'FOSAMAX PLUS D',
                                    'IBANDRONATE SODIUM', 'BONIVA', 'ATELVIA', 'ACTONEL',
                                    'RISEDRONATE SODIUM', 'ZOLEDRONIC ACID', 'ZOMETA', 'RECLAST',
                                    'PROLIA', 'FORTEO', 'RALOXIFENE HCL', 'EVISTA', sep = '|'),
      nsaid = stringr::str_c("FLECTOR","CATAFLAM","DICLOFENAC POTASSIUM","CAMBIA","ZIPSOR",
                             "PENNSAID","VOLTAREN","DICLOFENAC SODIUM","DICLOFENAC SODIUM ER",
                             "VOLTAREN-XR","DICLOFENAC SODIUM-MISOPROSTOL","ARTHROTEC 50",
                             "ARTHROTEC 75","ZORVOLEX","CATAFLAM","CELECOXIB","CELEBREX",
                             "DIFLUNISAL","ETODOLAC","ETODOLAC ER","NALFON","FENOPROFEN CALCIUM",
                             "FLURBIPROFEN","IBUPROFEN","DUEXIS","INDOMETHACIN","INDOCIN",
                             "KETOPROFEN","KETOROLAC TROMETHAMINE","MECLOFENAMATE SODIUM",
                             "MEFENAMIC ACID","PONSTEL","MELOXICAM","MOBIC","NABUMETONE",
                             "EC-NAPROSYN","NAPROSYN","NAPROXEN","NAPROXEN SODIUM","NAPRELAN",
                             "ANAPROX DS","ANAPROX","VIMOVO","OXAPROZIN","DAYPRO","PIROXICAM",
                             "FELDENE","SULINDAC","TOLMETIN SODIUM" , sep = '|'),
      nasal_steroids = stringr::str_c("DYMISTA","QNASL","BECONASE AQ","RHINOCORT AQUA",
                                      "BUDESONIDE","OMNARIS","ZETONNA","FLUNISOLIDE","VERAMYST",
                                      "FLONASE","FLUTICASONE PROPIONATE","NASONEX","NASACORT AQ",
                                      "TRIAMCINOLONE ACETONIDE", sep = '|'),
      neuropathic_pain = stringr::str_c("GRALISE", "GABAPENTIN", "NEURONTIN","HORIZANT","LYRICA",
                                        sep = '|'),
      opthalmic_antibiotic = stringr::str_c("BESIVANCE", "CIPROFLOXACIN HCL", "CILOXAN", "ZYMAXID",
                                            "GATIFLOXACIN", "LEVOFLOXACIN", "MOXEZA", "VIGAMOX",
                                            "OCUFLOX", "OFLOXACIN", sep = "|"),
      vaginal_cream = stringr::str_c("ESTRACE", "ESTRING", "VAGIFEM", "CLIMARA", "ALORA",
                                     "VIVELLE-DOT", "ESTROGEL", "PREMARIN", "MENEST",
                                     "ESTROPIPATE", sep = "|"),
      opioids = stringr::str_c("CAPITAL W-CODEINE", "TYLENOL-CODEINE NO.3", "TYLENOL-CODEINE NO.4",
                               "ACETAMINOPHEN-CODEINE", "ULTRACET", "TRAMADOL HCL-ACETAMINOPHEN",
                               "TREZIX", "BUTORPHANOL TARTRATE", "SYNALGOS-DC", "ASPIRIN-CAFFEINE-DIHYDROCODEIN",
                               "HYDROCODONE-ACETAMINOPHEN", "VICODIN ES", "VICODIN", "NORCO", "LORTAB",
                               "VICODIN HP", "LORCET 10-650", "ZAMICET", "XODOL 10-300", "LORCET PLUS",
                               "XODOL 7.5-300", "STAGESIC", "XODOL 5-300", "HYCET", "ZYDONE", "ZOLVIT",
                               "MAXIDONE", "CO-GESIC", "HYDROCODONE-IBUPROFEN", "IBUDONE", "REPREXAIN",
                               "VICOPROFEN", "XYLON 10", "HYDROMORPHONE HCL", "DILAUDID", "LEVORPHANOL TARTRATE",
                               "DEMEROL", "MEPERIDINE HCL", "MEPERITAB", "MORPHINE SULFATE", "ROXICODONE",
                               "OXECTA", "OXYCODONE CONCENTRATE", "OXYCODONE HCL", "ENDOCET", "PERCOCET",
                               "PRIMLEV", "ROXICET", "TYLOX", "MAGNACET", "OXYCODONE-ACETAMINOPHEN",
                               "PERCODAN", "ENDODAN", "OXYCODONE HCL-ASPIRIN", "OPANA", "OXYMORPHONE HCL",
                               "PENTAZOCINE-ACETAMINOPHEN", "PENTAZOCINE-NALOXONE HCL", "NUCYNTA", "ULTRAM",
                               "RYBIX ODT", "TRAMADOL HCL", "CODEINE SULFATE", "BUTRANS", "FENTANYL",
                               "DURAGESIC", "ZOHYDRO ER", "HYDROMORPHONE ER", "EXALGO", "DOLOPHINE HCL",
                               "METHADONE INTENSOL", "METHADONE HCL", "AVINZA", "KADIAN", "MS CONTIN",
                               "MORPHINE SULFATE ER", "OXYCONTIN", "OXYCODONE HCL ER", "XARTEMIS XR",
                               "OPANA ER", "OXYMORPHONE HCL ER", "NUCYNTA ER", "CONZIP", "ULTRAM ER",
                               "TRAMADOL HCL ER", "RYZOLT", "OXYCODONE HCL-IBUPROFEN", sep = "|"),
      oral_opioids = stringr::str_c("ZOHYDRO ER", "EXALGO", "HYDROMORPHONE ER", "AVINZA", "KADIAN",
                                    "MS CONTIN", "MORPHINE SULFATE ER", "OXYCONTIN", "OXYCODONE HCL ER",
                                    "XARTEMIS XR", "OPANA ER", "OXYMORPHONE HCL ER", "NUCYNTA ER",
                                    "ULTRAM ER", "TRAMADOL HCL ER", "DOLOPHINE HCL", "METHADONE INTENSOL",
                                    "METHADONE HCL", "CONZIP", "RYZOLT", sep = "|")
    ),

    partd_target = c(
      'crestor'   = 'CRESTOR',
      'zyvox'     = 'ZYVOX',
      'durezol'   = 'DUREZOL',
      'prolia'    = 'PROLIA',
      'voltaren'  = 'VOLTAREN',
      'nasonex'   = 'NASONEX',
      'lyrica'    = 'LYRICA',
      'lotemax'   = 'LOTEMAX',
      'besivance' = 'BESIVANCE',
      'estrace'   = 'ESTRACE',
      'oxycontin' = 'OXYCONTIN'
    ),

    open_payments_target = c(
      'crestor'   = 'CRESTOR',
      'zyvox'     = 'ZYVOX',
      'durezol'   = 'DUREZOL',
      'prolia'    = 'PROLIA',
      'voltaren'  = 'VOLTAREN Gel',
      'nasonex'   = 'NASONEX|NASONEX Respiratory',
      'lyrica'    = 'LYRICA',
      'lotemax'   = 'LOTEMAX|LOTEMAX GEL|LOTEMAX OINTMENT',
      'besivance' = 'BESIVANCE',
      'estrace'   = 'ESTRACE CREAM',
      'oxycontin' = 'OXYCONTIN'
     ),

    target_drug_manufacturer = c(
      'crestor'   = 'AstraZeneca Pharmaceuticals LP',
      'zyvox'     = 'Pfizer',
      'durezol'   = 'Alcon',
      'prolia'    = 'Amgen Inc.|Medtronic Sofamor Danek USA, Inc.',
      'voltaren'  = 'Endo Pharmaceuticals Inc.',
      'nasonex'   = 'Merck Sharp & Dohme Corporation|COMSORT, Inc',
      'lyrica'    = 'Pfizer',
      'lotemax'   = 'Bausch and Lomb Inc.|Valeant Pharmaceuticals North America LLC',
      'besivance' = 'Bausch and Lomb Inc.|Valeant Pharmaceuticals North America LLC',
      'estrace'   = 'Actavis Pharma Inc',
      'oxycontin' = 'Purdue Pharma L.P.'
    ),

    figure_drug_class_brand = c(
      'statins'                  = 'Crestor',
      'nsaid'                    = 'Voltaren',
      'nasal_steroids'           = 'Nasonex',
      'neuropathic_pain'         = 'Lyrica',
      'osteoporosis'             = 'Prolia',
      'opthalmic_corticosteroid' = 'Lotemax',
      'opthalmic_antibiotic'     = 'Besivance',
      'vaginal_cream'            = 'Estrace',
      'opioids'                  = 'Oxycontin',
      'oral_opioids'             = 'Oxycontin'
    ),

    figure_drug_class_formulary = c(
      'statins'                  = 'Rosuvastatin',
      'nsaid'                    = 'Diclofenac',
      'nasal_steroids'           = 'Mometasone',
      'neuropathic_pain'         = 'Pregabalin',
      'osteoporosis'             = 'Denosumab',
      'opthalmic_corticosteroid' = 'Loteprednol',
      'opthalmic_antibiotic'     = 'Besifloxacin',
      'vaginal_cream'            = 'Estradiol',
      'opioids'                  = 'Oxycodone',
      'oral_opiods'              = 'Oxycodone'
    )
  )
)
