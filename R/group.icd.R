#' @name group.icd
#' @aliases chapter.icd
#' @aliases subchapter.icd
#'
#'
#'
#'
#' @title Higher level groupings for ICD.10 codes
#'
#' @description For a  variable containing ICD-10 Codes what are the associated higher level groupings? \cr
#'
#' The International Classification of Diseases, Tenth Revision (ICD-10) is a comprehensive system of codes used to classify and standardize medical diagnoses, symptoms, and procedures. Developed by the World Health Organization (WHO), ICD-10 provides a universal language for healthcare professionals to communicate and record medical information.
#' Due to underlying assumptions of research questions and nuances in healthcare, a method for grouping codes in one scenario may not be suitable in another scenario.  As such, grouping ICD-10 codes into higher levels is frequently inconsistent. This package offers several grouping levels that may be a useful starting point for researchers.
#'
#' @param x `character` An ICD code, for example 'R85.1'.
#'
#' @note Can pass a vector. \cr
#'
#'  See also
#'  [WHO - ICD-10 Version:2019](https://icd.who.int/browse10/2019/en)
#'  [ICD10data.com](https://www.icd10data.com/ICD10CM/Codes)
#'
#'  @examples
#'
#'  df <- data.frame(ID = c( 1,2,3,4,5), my.var = c('A3.0', 'R85.1', 'F64', 'U10.2', 'O9A'))
#'
#'
#'  df$chapter <- chapter.icd(df$my.var)
#'
#'
#'
#'
# chapter.icd ----
#' @rdname group.icd
#' @usage chapter.icd(x)
#' @export
chapter.icd <- function( x ){

  length = length(x)

  # create an empty result

  y = c(NA)

  # create a for loop for vecotisation

  for ( i in 1:length){

  # separate the diagnosis code

  ICD_prefix = stringr::str_extract(x[i], '[A-Z]+')

  ICD_number =  as.numeric(stringr::str_remove_all(x[i], '[A-Z]+') )

  # groupings for output

    y[i] = if ( ( ICD_prefix    == 'A'      |   ICD_prefix    == 'B' ) & ( ICD_number     >=   0 & ICD_number < 100 ) ){                                                     'Certain infectious and parasitic diseases'
 } else if ( ( ICD_prefix    == 'C'      &   ICD_number    >=  0    &   ICD_number      < 100 ) | ( ICD_prefix    == 'D'   &   ICD_number >= 0 & ICD_number < 50 )){      'Neoplasms'
 } else if ( ( ICD_prefix    == 'D'      &   ICD_number    >= 50    &   ICD_number      < 100 ) ){                                                                        'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'
 } else if ( ( ICD_prefix    == 'E'      &   ICD_number    >=  0    &   ICD_number      <  90 ) ){                                                                        'Endocrine, nutritional and metabolic diseases'
 } else if ( ( ICD_prefix    == 'F'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Mental, Behavioral and Neurodevelopmental disorders'
 } else if ( ( ICD_prefix    == 'G'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the nervous system'
 } else if ( ( ICD_prefix    == 'H'      &   ICD_number    >=  0    &   ICD_number      <  60 ) ){                                                                        'Diseases of the eye and adnexa'
 } else if ( ( ICD_prefix    == 'H'      &   ICD_number    >= 60    &   ICD_number      <  96 ) ){                                                                        'Diseases of the ear and mastoid process'
 } else if ( ( ICD_prefix    == 'I'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the circulatory system'
 } else if ( ( ICD_prefix    == 'J'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the respiratory system'
 } else if ( ( ICD_prefix    == 'K'      &   ICD_number    >=  0    &   ICD_number      <  96 ) ){                                                                        'Diseases of the digestive system'
 } else if ( ( ICD_prefix    == 'L'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the skin and subcutaneous tissue'
 } else if ( ( ICD_prefix    == 'M'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the musculoskeletal system and connective tissue'
 } else if ( ( ICD_prefix    == 'N'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Diseases of the genitourinary system'
 } else if ( ( x[i]             == 'O9A'  ) | ( ICD_prefix    == 'O'   &   ICD_number     >=   0 & ICD_number < 100 ) ){                                                     'Pregnancy, childbirth and the puerperium'
 } else if ( ( ICD_prefix    == 'P'      &   ICD_number    >=  0    &   ICD_number      <  97 ) ){                                                                        'Certain conditions originating in the perinatal period'
 } else if ( ( ICD_prefix    == 'Q'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Congenital malformations, deformations and chromosomal abnormalities'
 } else if ( ( ICD_prefix    == 'R'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified'
 } else if ( ( ICD_prefix    == 'S'      |   ICD_prefix    == 'T' ) & ( ICD_number     >=   0 & ICD_number < 100 ) ){                                                     'Injury, poisoning and certain other consequences of external causes'
 } else if ( ( ICD_prefix    == 'U'      &   ICD_number    >=  0    &   ICD_number      <  86 ) ){                                                                        'Codes for special purposes'
 } else if ( ( ICD_prefix    == 'V'      |   ICD_prefix    == 'W'   |   ICD_prefix     ==  'X' |  ICD_prefix    == 'Y' ) & ( ICD_number >=  0 & ICD_number < 100 ) ){     'External causes of morbidity'
 } else if ( ( ICD_prefix    == 'Z'      &   ICD_number    >=  0    &   ICD_number      < 100 ) ){                                                                        'Factors influencing health status and contact with health services'

 } # end if statements

  } # end for loop

  return(y)

 } # end chapter.icd()



# ---------------------------------------------------------------------------------------------------------------------------



# subchapter.icd ----
#' @rdname group.icd
#' @usage subchapter.icd(x)
#' @export
subchapter.icd <- function( x ){

  length = length(x)

  # create an empty result

  y = c(NA)

  # create a for loop for vecotisation

  for ( i in 1:length){

    # separate the diagnosis code

    ICD_prefix = stringr::str_extract(x[i], '[A-Z]+')

    ICD_number =  as.numeric(stringr::str_remove_all(x[i], '[A-Z]+') )

    # groupings for output


    y[i] = if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Intestinal infectious diseases"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 15 & ICD_number < 20 ) ){  "Tuberculosis"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 20 & ICD_number < 29 ) ){  "Certain zoonotic bacterial diseases"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 30 & ICD_number < 50 ) ){  "Other bacterial diseases"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 50 & ICD_number < 65 ) ){  "Infections with a predominantly sexual mode of transmission"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 65 & ICD_number < 70 ) ){  "Other spirochetal diseases"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 70 & ICD_number < 75 ) ){  "Other diseases caused by chlamydiae"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 75 & ICD_number < 80 ) ){  "Rickettsioses"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Viral and prion infections of the central nervous system"
    } else if ( ( ICD_prefix == 'A' ) & ( ICD_number >= 90 & ICD_number < 100 ) ){  "Arthropod-borne viral fevers and viral hemorrhagic fevers"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Viral infections characterized by skin and mucous membrane lesions"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 10 & ICD_number < 11 ) ){  "Other human herpesviruses"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 15 & ICD_number < 20 ) ){  "Viral hepatitis"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 20 & ICD_number < 21 ) ){  "Human immunodeficiency virus [HIV] disease"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 25 & ICD_number < 35 ) ){  "Other viral diseases"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 35 & ICD_number < 50 ) ){  "Mycoses"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 50 & ICD_number < 65 ) ){  "Protozoal diseases"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 65 & ICD_number < 84 ) ){  "Helminthiases"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 85 & ICD_number < 90 ) ){  "Pediculosis, acariasis and other infestations"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 90 & ICD_number < 95 ) ){  "Sequelae of infectious and parasitic diseases"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 95 & ICD_number < 98 ) ){  "Bacterial and viral infectious agents"
    } else if ( ( ICD_prefix == 'B' ) & ( ICD_number >= 99 & ICD_number < 100 ) ){  "Other infectious diseases"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 0 & ICD_number < 15 ) ){  "Malignant neoplasms of lip, oral cavity and pharynx"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 15 & ICD_number < 27 ) ){  "Malignant neoplasms of digestive organs"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Malignant neoplasms of respiratory and intrathoracic organs"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 40 & ICD_number < 42 ) ){  "Malignant neoplasms of bone and articular cartilage"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 43 & ICD_number < 45 ) ){  "Melanoma and other malignant neoplasms of skin"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 45 & ICD_number < 50 ) ){  "Malignant neoplasms of mesothelial and soft tissue"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 50 & ICD_number < 51 ) ){  "Malignant neoplasms of breast"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 51 & ICD_number < 59 ) ){  "Malignant neoplasms of female genital organs"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 60 & ICD_number < 64 ) ){  "Malignant neoplasms of male genital organs"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 64 & ICD_number < 69 ) ){  "Malignant neoplasms of urinary tract"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 69 & ICD_number < 73 ) ){  "Malignant neoplasms of eye, brain and other parts of central nervous system"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 7 & ICD_number < 8 ) ){  "Malignant neuroendocrine tumors"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 7 & ICD_number < 8 ) ){  "Secondary neuroendocrine tumors"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 73 & ICD_number < 76 ) ){  "Malignant neoplasms of thyroid and other endocrine glands"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 76 & ICD_number < 81 ) ){  "Malignant neoplasms of ill-defined, other secondary and unspecified sites"
    } else if ( ( ICD_prefix == 'C' ) & ( ICD_number >= 81 & ICD_number < 97 ) ){  "Malignant neoplasms of lymphoid, hematopoietic and related tissue"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "In situ neoplasms"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 10 & ICD_number < 37 ) ){  "Benign neoplasms, except benign neuroendocrine tumors"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 3 & ICD_number < 4 ) ){  "Benign neuroendocrine tumors"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 37 & ICD_number < 49 ) ){  "Neoplasms of uncertain behavior, polycythemia vera and myelodysplastic syndromes"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 49 & ICD_number < 50 ) ){  "Neoplasms of unspecified behavior"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 50 & ICD_number < 54 ) ){  "Nutritional anemias"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 55 & ICD_number < 60 ) ){  "Hemolytic anemias"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 60 & ICD_number < 65 ) ){  "Aplastic and other anemias and other bone marrow failure syndromes"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 65 & ICD_number < 70 ) ){  "Coagulation defects, purpura and other hemorrhagic conditions"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 70 & ICD_number < 78 ) ){  "Other disorders of blood and blood-forming organs"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 78 & ICD_number < 79 ) ){  "Intraoperative and postprocedural complications of the spleen"
    } else if ( ( ICD_prefix == 'D' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Certain disorders involving the immune mechanism"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 0 & ICD_number < 8 ) ){  "Disorders of thyroid gland"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 15 & ICD_number < 17 ) ){  "Other disorders of glucose regulation and pancreatic internal secretion"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 20 & ICD_number < 36 ) ){  "Disorders of other endocrine glands"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 36 & ICD_number < 37 ) ){  "Intraoperative complications of endocrine system"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 40 & ICD_number < 47 ) ){  "Malnutrition"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 50 & ICD_number < 65 ) ){  "Other nutritional deficiencies"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 65 & ICD_number < 69 ) ){  "Overweight, obesity and other hyperalimentation"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 70 & ICD_number < 89 ) ){  "Metabolic disorders"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 8 & ICD_number < 14 ) ){  "Diabetes mellitus"
    } else if ( ( ICD_prefix == 'E' ) & ( ICD_number >= 89 & ICD_number < 90 ) ){  "Postprocedural endocrine and metabolic complications and disorders, not elsewhere classified"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 1 & ICD_number < 10 ) ){  "Mental disorders due to known physiological conditions"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 10 & ICD_number < 20 ) ){  "Mental and behavioral disorders due to psychoactive substance use"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 20 & ICD_number < 30 ) ){  "Schizophrenia, schizotypal, delusional, and other non-mood psychotic disorders"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Mood [affective] disorders"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 40 & ICD_number < 49 ) ){  "Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 50 & ICD_number < 60 ) ){  "Behavioral syndromes associated with physiological disturbances and physical factors"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 60 & ICD_number < 70 ) ){  "Disorders of adult personality and behavior"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Intellectual disabilities"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Pervasive and specific developmental disorders"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 90 & ICD_number < 99 ) ){  "Behavioral and emotional disorders with onset usually occurring in childhood and adolescence"
    } else if ( ( ICD_prefix == 'F' ) & ( ICD_number >= 99 & ICD_number < 100 ) ){  "Unspecified mental disorder"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Inflammatory diseases of the central nervous system"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 10 & ICD_number < 15 ) ){  "Systemic atrophies primarily affecting the central nervous system"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 20 & ICD_number < 27 ) ){  "Extrapyramidal and movement disorders"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 30 & ICD_number < 33 ) ){  "Other degenerative diseases of the nervous system"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 35 & ICD_number < 38 ) ){  "Demyelinating diseases of the central nervous system"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 40 & ICD_number < 48 ) ){  "Episodic and paroxysmal disorders"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 50 & ICD_number < 60 ) ){  "Nerve, nerve root and plexus disorders"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 60 & ICD_number < 66 ) ){  "Polyneuropathies and other disorders of the peripheral nervous system"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 70 & ICD_number < 74 ) ){  "Diseases of myoneural junction and muscle"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 80 & ICD_number < 84 ) ){  "Cerebral palsy and other paralytic syndromes"
    } else if ( ( ICD_prefix == 'G' ) & ( ICD_number >= 89 & ICD_number < 100 ) ){  "Other disorders of the nervous system"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 0 & ICD_number < 6 ) ){  "Disorders of eyelid, lacrimal system and orbit"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 10 & ICD_number < 12 ) ){  "Disorders of conjunctiva"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 15 & ICD_number < 23 ) ){  "Disorders of sclera, cornea, iris and ciliary body"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 25 & ICD_number < 29 ) ){  "Disorders of lens"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 30 & ICD_number < 37 ) ){  "Disorders of choroid and retina"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 40 & ICD_number < 43 ) ){  "Glaucoma"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 43 & ICD_number < 45 ) ){  "Disorders of vitreous body and globe"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 46 & ICD_number < 48 ) ){  "Disorders of optic nerve and visual pathways"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 49 & ICD_number < 53 ) ){  "Disorders of ocular muscles, binocular movement, accommodation and refraction"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 53 & ICD_number < 55 ) ){  "Visual disturbances and blindness"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 55 & ICD_number < 58 ) ){  "Other disorders of eye and adnexa"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 59 & ICD_number < 60 ) ){  "Intraoperative and postprocedural complications and disorders of eye and adnexa, not elsewhere classified"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 60 & ICD_number < 63 ) ){  "Diseases of external ear"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 65 & ICD_number < 76 ) ){  "Diseases of middle ear and mastoid"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 80 & ICD_number < 84 ) ){  "Diseases of inner ear"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 90 & ICD_number < 95 ) ){  "Other disorders of ear"
    } else if ( ( ICD_prefix == 'H' ) & ( ICD_number >= 95 & ICD_number < 96 ) ){  "Intraoperative and postprocedural complications and disorders of ear and mastoid process, not elsewhere classified"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 0 & ICD_number < 3 ) ){  "Acute rheumatic fever"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 10 & ICD_number < 17 ) ){  "Hypertensive diseases"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 20 & ICD_number < 26 ) ){  "Ischemic heart diseases"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 26 & ICD_number < 29 ) ){  "Pulmonary heart disease and diseases of pulmonary circulation"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 30 & ICD_number < 6 ) ){  "Other forms of heart disease"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 5 & ICD_number < 10 ) ){  "Chronic rheumatic heart diseases"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 60 & ICD_number < 70 ) ){  "Cerebrovascular diseases"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Diseases of arteries, arterioles and capillaries"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Diseases of veins, lymphatic vessels and lymph nodes, not elsewhere classified"
    } else if ( ( ICD_prefix == 'I' ) & ( ICD_number >= 95 & ICD_number < 100 ) ){  "Other and unspecified disorders of the circulatory system"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 0 & ICD_number < 7 ) ){  "Acute upper respiratory infections"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 20 & ICD_number < 23 ) ){  "Other acute lower respiratory infections"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Other diseases of upper respiratory tract"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 40 & ICD_number < 48 ) ){  "Chronic lower respiratory diseases"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 60 & ICD_number < 71 ) ){  "Lung diseases due to external agents"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 80 & ICD_number < 85 ) ){  "Other respiratory diseases principally affecting the interstitium"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 85 & ICD_number < 87 ) ){  "Suppurative and necrotic conditions of the lower respiratory tract"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 9 & ICD_number < 19 ) ){  "Influenza and pneumonia"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 90 & ICD_number < 95 ) ){  "Other diseases of the pleura"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 95 & ICD_number < 96 ) ){  "Intraoperative and postprocedural complications and disorders of respiratory system, not elsewhere classified"
    } else if ( ( ICD_prefix == 'J' ) & ( ICD_number >= 96 & ICD_number < 100 ) ){  "Other diseases of the respiratory system"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 0 & ICD_number < 15 ) ){  "Diseases of oral cavity and salivary glands"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 20 & ICD_number < 32 ) ){  "Diseases of esophagus, stomach and duodenum"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 35 & ICD_number < 39 ) ){  "Diseases of appendix"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 40 & ICD_number < 47 ) ){  "Hernia"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 50 & ICD_number < 53 ) ){  "Noninfective enteritis and colitis"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 55 & ICD_number < 65 ) ){  "Other diseases of intestines"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 65 & ICD_number < 69 ) ){  "Diseases of peritoneum and retroperitoneum"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 70 & ICD_number < 78 ) ){  "Diseases of liver"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 80 & ICD_number < 88 ) ){  "Disorders of gallbladder, biliary tract and pancreas"
    } else if ( ( ICD_prefix == 'K' ) & ( ICD_number >= 90 & ICD_number < 96 ) ){  "Other diseases of the digestive system"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 0 & ICD_number < 9 ) ){  "Infections of the skin and subcutaneous tissue"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 10 & ICD_number < 15 ) ){  "Bullous disorders"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 20 & ICD_number < 31 ) ){  "Dermatitis and eczema"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 40 & ICD_number < 46 ) ){  "Papulosquamous disorders"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 49 & ICD_number < 55 ) ){  "Urticaria and erythema"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 55 & ICD_number < 60 ) ){  "Radiation-related disorders of the skin and subcutaneous tissue"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 60 & ICD_number < 76 ) ){  "Disorders of skin appendages"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 76 & ICD_number < 77 ) ){  "Intraoperative and postprocedural complications of skin and subcutaneous tissue"
    } else if ( ( ICD_prefix == 'L' ) & ( ICD_number >= 80 & ICD_number < 100 ) ){  "Other disorders of the skin and subcutaneous tissue"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 0 & ICD_number < 3 ) ){  "Infectious arthropathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 15 & ICD_number < 20 ) ){  "Osteoarthritis"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 20 & ICD_number < 26 ) ){  "Other joint disorders"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 26 & ICD_number < 28 ) ){  "Dentofacial anomalies [including malocclusion] and other disorders of jaw"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 30 & ICD_number < 37 ) ){  "Systemic connective tissue disorders"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 4 & ICD_number < 5 ) ){  "Autoinflammatory syndromes"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 40 & ICD_number < 44 ) ){  "Deforming dorsopathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 45 & ICD_number < 50 ) ){  "Spondylopathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 5 & ICD_number < 15 ) ){  "Inflammatory polyarthropathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 50 & ICD_number < 55 ) ){  "Other dorsopathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 60 & ICD_number < 64 ) ){  "Disorders of muscles"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 65 & ICD_number < 68 ) ){  "Disorders of synovium and tendon"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Other soft tissue disorders"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 80 & ICD_number < 86 ) ){  "Disorders of bone density and structure"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 86 & ICD_number < 91 ) ){  "Other osteopathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 91 & ICD_number < 95 ) ){  "Chondropathies"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 95 & ICD_number < 96 ) ){  "Other disorders of the musculoskeletal system and connective tissue"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 96 & ICD_number < 97 ) ){  "Intraoperative and postprocedural complications and disorders of musculoskeletal system, not elsewhere classified"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 97 & ICD_number < 98 ) ){  "Periprosthetic fracture around internal prosthetic joint"
    } else if ( ( ICD_prefix == 'M' ) & ( ICD_number >= 99 & ICD_number < 100 ) ){  "Biomechanical lesions, not elsewhere classified"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 0 & ICD_number < 9 ) ){  "Glomerular diseases"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 10 & ICD_number < 17 ) ){  "Renal tubulo-interstitial diseases"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 17 & ICD_number < 20 ) ){  "Acute kidney failure and chronic kidney disease"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 20 & ICD_number < 24 ) ){  "Urolithiasis"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 25 & ICD_number < 30 ) ){  "Other disorders of kidney and ureter"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Other diseases of the urinary system"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 40 & ICD_number < 54 ) ){  "Diseases of male genital organs"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 60 & ICD_number < 66 ) ){  "Disorders of breast"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 70 & ICD_number < 78 ) ){  "Inflammatory diseases of female pelvic organs"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 80 & ICD_number < 99 ) ){  "Noninflammatory disorders of female genital tract"
    } else if ( ( ICD_prefix == 'N' ) & ( ICD_number >= 99 & ICD_number < 100 ) ){  "Intraoperative and postprocedural complications and disorders of genitourinary system, not elsewhere classified"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 0 & ICD_number < 9 ) ){  "Pregnancy with abortive outcome"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 10 & ICD_number < 17 ) ){  "Edema, proteinuria and hypertensive disorders in pregnancy, childbirth and the puerperium"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 20 & ICD_number < 30 ) ){  "Other maternal disorders predominantly related to pregnancy"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 30 & ICD_number < 49 ) ){  "Maternal care related to the fetus and amniotic cavity and possible delivery problems"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 60 & ICD_number < 78 ) ){  "Complications of labor and delivery"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 80 & ICD_number < 83 ) ){  "Encounter for delivery"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 85 & ICD_number < 93 ) ){  "Complications predominantly related to the puerperium"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 9 & ICD_number < 10 ) ){  "Supervision of high risk pregnancy"
    } else if ( ( ICD_prefix == 'O' ) & ( ICD_number >= 94 & ICD_number < 10 ) ){  "Other obstetric conditions, not elsewhere classified"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 0 & ICD_number < 5 ) ){  "Newborn affected by maternal factors and by complications of pregnancy, labor, and delivery"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 10 & ICD_number < 16 ) ){  "Birth trauma"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 19 & ICD_number < 30 ) ){  "Respiratory and cardiovascular disorders specific to the perinatal period"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 35 & ICD_number < 40 ) ){  "Infections specific to the perinatal period"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 5 & ICD_number < 9 ) ){  "Disorders of newborn related to length of gestation and fetal growth"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 50 & ICD_number < 62 ) ){  "Hemorrhagic and hematological disorders of newborn"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 70 & ICD_number < 75 ) ){  "Transitory endocrine and metabolic disorders specific to newborn"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 76 & ICD_number < 79 ) ){  "Digestive system disorders of newborn"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 80 & ICD_number < 84 ) ){  "Conditions involving the integument and temperature regulation of newborn"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 84 & ICD_number < 85 ) ){  "Other problems with newborn"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 9 & ICD_number < 10 ) ){  "Abnormal findings on neonatal screening"
    } else if ( ( ICD_prefix == 'P' ) & ( ICD_number >= 90 & ICD_number < 97 ) ){  "Other disorders originating in the perinatal period"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 0 & ICD_number < 8 ) ){  "Congenital malformations of the nervous system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 10 & ICD_number < 19 ) ){  "Congenital malformations of eye, ear, face and neck"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 20 & ICD_number < 29 ) ){  "Congenital malformations of the circulatory system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 30 & ICD_number < 35 ) ){  "Congenital malformations of the respiratory system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 35 & ICD_number < 38 ) ){  "Cleft lip and cleft palate"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 38 & ICD_number < 46 ) ){  "Other congenital malformations of the digestive system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 50 & ICD_number < 57 ) ){  "Congenital malformations of genital organs"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 60 & ICD_number < 65 ) ){  "Congenital malformations of the urinary system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 65 & ICD_number < 80 ) ){  "Congenital malformations and deformations of the musculoskeletal system"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Other congenital malformations"
    } else if ( ( ICD_prefix == 'Q' ) & ( ICD_number >= 90 & ICD_number < 100 ) ){  "Chromosomal abnormalities, not elsewhere classified"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Symptoms and signs involving the circulatory and respiratory systems"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 10 & ICD_number < 20 ) ){  "Symptoms and signs involving the digestive system and abdomen"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 20 & ICD_number < 24 ) ){  "Symptoms and signs involving the skin and subcutaneous tissue"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 25 & ICD_number < 30 ) ){  "Symptoms and signs involving the nervous and musculoskeletal systems"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Symptoms and signs involving the genitourinary system"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 40 & ICD_number < 47 ) ){  "Symptoms and signs involving cognition, perception, emotional state and behavior"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 47 & ICD_number < 50 ) ){  "Symptoms and signs involving speech and voice"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 50 & ICD_number < 70 ) ){  "General symptoms and signs"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Abnormal findings on examination of blood, without diagnosis"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 80 & ICD_number < 83 ) ){  "Abnormal findings on examination of urine, without diagnosis"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 83 & ICD_number < 90 ) ){  "Abnormal findings on examination of other body fluids, substances and tissues, without diagnosis"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 90 & ICD_number < 95 ) ){  "Abnormal findings on diagnostic imaging and in function studies, without diagnosis"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 97 & ICD_number < 98 ) ){  "Abnormal tumor markers"
    } else if ( ( ICD_prefix == 'R' ) & ( ICD_number >= 99 & ICD_number < 100 ) ){  "Ill-defined and unknown cause of mortality"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Injuries to the head"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 10 & ICD_number < 20 ) ){  "Injuries to the neck"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 20 & ICD_number < 30 ) ){  "Injuries to the thorax"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Injuries to the abdomen, lower back, lumbar spine, pelvis and external genitals"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 40 & ICD_number < 50 ) ){  "Injuries to the shoulder and upper arm"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 50 & ICD_number < 60 ) ){  "Injuries to the elbow and forearm"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 60 & ICD_number < 70 ) ){  "Injuries to the wrist, hand and fingers"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Injuries to the hip and thigh"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Injuries to the knee and lower leg"
    } else if ( ( ICD_prefix == 'S' ) & ( ICD_number >= 90 & ICD_number < 100 ) ){  "Injuries to the ankle and foot"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 14 & ICD_number < 15 ) ){  "Injury of unspecified body region"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 15 & ICD_number < 20 ) ){  "Effects of foreign body entering through natural orifice"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 20 & ICD_number < 26 ) ){  "Burns and corrosions of external body surface, specified by site"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 26 & ICD_number < 29 ) ){  "Burns and corrosions confined to eye and internal organs"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 30 & ICD_number < 33 ) ){  "Burns and corrosions of multiple and unspecified body regions"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 33 & ICD_number < 35 ) ){  "Frostbite"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 36 & ICD_number < 51 ) ){  "Poisoning by, adverse effect of and underdosing of drugs, medicaments and biological substances"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 51 & ICD_number < 66 ) ){  "Toxic effects of substances chiefly nonmedicinal as to source"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 66 & ICD_number < 79 ) ){  "Other and unspecified effects of external causes"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 7 & ICD_number < 8 ) ){  "Injuries involving multiple body regions"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 79 & ICD_number < 80 ) ){  "Certain early complications of trauma"
    } else if ( ( ICD_prefix == 'T' ) & ( ICD_number >= 80 & ICD_number < 89 ) ){  "Complications of surgical and medical care, not elsewhere classified"
    } else if ( ( ICD_prefix == 'U' ) & ( ICD_number >= 0 & ICD_number < 50 ) ){  "Provisional assignment of new diseases of uncertain etiology or emergency use"
    } else if ( ( ICD_prefix == 'U' ) & ( ICD_number >= 50 & ICD_number < 86 ) ){  "Provisional assignment of new diseases of uncertain etiology or emergency use"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Pedestrian injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 10 & ICD_number < 20 ) ){  "Pedal cycle rider injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 20 & ICD_number < 30 ) ){  "Motorcycle rider injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Occupant of three-wheeled motor vehicle injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 40 & ICD_number < 50 ) ){  "Car occupant injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 50 & ICD_number < 60 ) ){  "Occupant of pick-up truck or van injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 60 & ICD_number < 70 ) ){  "Occupant of heavy transport vehicle injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 70 & ICD_number < 80 ) ){  "Bus occupant injured in transport accident"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 80 & ICD_number < 90 ) ){  "Other land transport accidents"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 90 & ICD_number < 95 ) ){  "Water transport accidents"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 95 & ICD_number < 98 ) ){  "Air and space transport accidents"
    } else if ( ( ICD_prefix == 'V' ) & ( ICD_number >= 98 & ICD_number < 100 ) ){  "Other and unspecified transport accidents"
    } else if ( ( ICD_prefix == 'W' ) & ( ICD_number >= 0 & ICD_number < 20 ) ){  "Slipping, tripping, stumbling and falls"
    } else if ( ( ICD_prefix == 'W' ) & ( ICD_number >= 20 & ICD_number < 50 ) ){  "Exposure to inanimate mechanical forces"
    } else if ( ( ICD_prefix == 'W' ) & ( ICD_number >= 50 & ICD_number < 65 ) ){  "Exposure to animate mechanical forces"
    } else if ( ( ICD_prefix == 'W' ) & ( ICD_number >= 65 & ICD_number < 75 ) ){  "Accidental non-transport drowning and submersion"
    } else if ( ( ICD_prefix == 'W' ) & ( ICD_number >= 85 & ICD_number < 100 ) ){  "Exposure to electric current, radiation and extreme ambient air temperature and pressure"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 0 & ICD_number < 9 ) ){  "Exposure to smoke, fire and flames"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 10 & ICD_number < 20 ) ){  "Contact with heat and hot substances"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Exposure to forces of nature"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 50 & ICD_number < 51 ) ){  "Overexertion and strenuous or repetitive movements"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 52 & ICD_number < 59 ) ){  "Accidental exposure to other specified factors"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 71 & ICD_number < 84 ) ){  "Intentional self-harm"
    } else if ( ( ICD_prefix == 'X' ) & ( ICD_number >= 92 & ICD_number < 100 ) ){  "Assault"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 21 & ICD_number < 34 ) ){  "Event of undetermined intent"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 35 & ICD_number < 39 ) ){  "Legal intervention, operations of war, military operations, and terrorism"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 62 & ICD_number < 70 ) ){  "Misadventures to patients during surgical and medical care"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 70 & ICD_number < 83 ) ){  "Medical devices associated with adverse incidents in diagnostic and therapeutic use"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 83 & ICD_number < 85 ) ){  "Surgical and other medical procedures as the cause of abnormal reaction of the patient, or of later complication, without mention of misadventure at the time of the procedure"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 90 & ICD_number < 100 ) ){  "Supplementary factors related to causes of morbidity classified elsewhere"
    } else if ( ( ICD_prefix == 'Y' ) & ( ICD_number >= 0 & ICD_number < 10 ) ){  "Assault"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 0 & ICD_number < 14 ) ){  "Persons encountering health services for examinations"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 14 & ICD_number < 16 ) ){  "Genetic carrier and genetic susceptibility to disease"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 16 & ICD_number < 17 ) ){  "Resistance to antimicrobial drugs"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 17 & ICD_number < 18 ) ){  "Estrogen receptor status"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 18 & ICD_number < 19 ) ){  "Retained foreign body fragments"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 19 & ICD_number < 20 ) ){  "Hormone sensitivity malignancy status"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 20 & ICD_number < 30 ) ){  "Persons with potential health hazards related to communicable diseases"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 30 & ICD_number < 40 ) ){  "Persons encountering health services in circumstances related to reproduction"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 40 & ICD_number < 54 ) ){  "Encounters for other specific health care"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 55 & ICD_number < 66 ) ){  "Persons with potential health hazards related to socioeconomic and psychosocial circumstances"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 66 & ICD_number < 67 ) ){  "Do not resuscitate status"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 67 & ICD_number < 68 ) ){  "Blood type"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 68 & ICD_number < 69 ) ){  "Body mass index (BMI)"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 69 & ICD_number < 77 ) ){  "Persons encountering health services in other circumstances"
    } else if ( ( ICD_prefix == 'Z' ) & ( ICD_number >= 77 & ICD_number < 100 ) ){  "Persons with potential health hazards related to family and personal history and certain conditions influencing health status"

    } # end if statements

  } # end for loop

  return(y)

} # end subchapter.icd()





