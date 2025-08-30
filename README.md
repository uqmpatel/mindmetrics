# mindmetrics


This package is written for r, to help process and understand large public health data sets, particularly related to psychology and mental health.

***

### Clinical Significance

While a p value tells us about the probability of observing a change of certain size, and a Standardised mean difference (SMD ~ Cohen's d/Hedge's g) indicates a probabilistic or statistical magnitude, a Reliable Change Index (RCI) discerns whether a change score measurement is a true positive or negative, according to; a pre-specified level of confidence, the error of an instrument, and a normative distribution.

Extending the RCI to clinical significance suggests that changes in instrument scores larger than an RCI, would be perceptible by an individual/patient, therapist, or significant other. It is a common metric to estimate a threshold required to meet levels of clinical significance in psychometric studies, and compatible with the concept of a Minimally Clinically Important Difference (MCID).

Refer to the article by [Jacobson & Truax (1991)]((https://pubmed.ncbi.nlm.nih.gov/2002127/)) for further explanation.

### International Classification of Diagnosis codes (ICD Codes)

The International Classification of Diseases, Tenth Revision (ICD-10) is a comprehensive system of codes used to classify and standardize medical diagnoses, symptoms, and procedures. Developed by the World Health Organization (WHO), ICD-10 provides a universal language for healthcare professionals to communicate and record medical information.

The benefits of an international classification system are:

* Improved data quality: Standardized codes ensure accurate and consistent data collection and analysis.
* Enhanced patient care: ICD-10 codes facilitate communication and coordination of care among healthcare professionals.
* Increased research and public health opportunities: Standardized codes enable researchers and public health officials to collect and analyze data on a global scale.

Due to underlying assumptions of research questions and nuances in healthcare, a method for grouping codes in one scenario may not be suitable in another scenario.  As such, grouping ICD-10 codes into higher levels is frequently inconsistent. This package offers several grouping levels that may be a useful starting point for researchers.
