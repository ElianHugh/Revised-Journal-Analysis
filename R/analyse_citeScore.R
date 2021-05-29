box::use(
    dplyr[...],
    fuzzyjoin[stringdist_left_join],
    glue[glue],
    purrr[map_df],
    enumr[enum],
    . / helper[coalesce_join, checkmatch],
)

#' @export
analyse_citeScore <- function(topFactor, citeScore) {

    disc <- enum(
        area1 = "Agricultural and Biological Sciences",
        area2 = "Arts and Humanities",
        area3 = "Biochemistry, Genetics and Molecular Biology",
        area4 = "Business, Management and Accounting",
        area5 = "Chemical Engineering",
        area6 = "Chemistry",
        area7 = "Computer Science",
        area8 = "Decision Sciences",
        area9 = "Dentistry",
        area10 = "Earth and Planetary Sciences",
        area11 = "Economics, Econometrics and Finance",
        area12 = "Energy",
        area13 = "Engineering",
        area14 = "Environmental Science",
        area15 = "Health Professions",
        area16 = "Immunology and Microbiology",
        area17 = "Materials Science",
        area18 = "Mathematics",
        area19 = "Medicine",
        area20 = "Neuroscience",
        area21 = "Nursing",
        area22 = "Pharmacology, Toxicology and Pharmaceutics",
        area23 = "Physics and Astronomy",
        area24 = "Psychology",
        area25 = "Social Sciences",
        area26 = "Veterinary",
        area27 = "Law"
    )

    grouped_disc <- enum(
        area1 = "Life Sciences",
        area2 = "Mathematics & Comp. Sciences",
        area3 = "Engineering",
        area4 = "Medical & Health Sciences",
        area5 = "ASSH",
        area6 = "Business & Law",
        area7 = "Psych. & Cog. Sciences",
        area8 = "Physical Sciences"
    )

    # Join data
    df <- left_join(topFactor, citeScore)

    # Retry
    df <- topFactor %>%
        coalesce_join(df, by = "ISSN")

    iterateOverISSN <- function(df, citeScore, iteration) {
        message(glue("Iteration {iteration}"))
        toRemove <- df[is.na(df$`Scopus Sub-Subject Area`), ] %>%
            select(
                Title,
                ISSN,
                Publisher,
                DataTransparency,
                AnalysisTransparency,
                MaterialsTransparency,
                DesignAnalysis,
                Preregistration,
                Replication,
                AnalysisPreReg,
                RegRepPubBias,
                DataTransparency,
                DataCitation,
                Badges
            )
        df <- anti_join(df, toRemove, by = "Title")

        closestMatches <- stringdist_left_join(
            toRemove,
            citeScore,
            by = "Title",
            max_dist = iteration,
            distance_col = "Distance"
        ) %>%
            group_by(Title.x) %>%
            top_n(1, desc(Distance)) %>%
            ungroup() %>%
            rename(
                Title = Title.x,
                MatchTitle = Title.y,
                ISSN = ISSN.x,
                Publisher = Publisher.x
            ) %>%
            select(-c(Publisher.y, ISSN.y))

        df <- coalesce_join(df, closestMatches, by = "Title") %>%
            select(Title, MatchTitle, everything()) %>%
            mutate(MatchTitle = {
                ifelse(is.na(.$MatchTitle), .$Title, .$MatchTitle)
            }) %>%
            rowwise() %>%
            mutate(Keep = checkmatch(Title, MatchTitle)) %>%
            filter(Keep == TRUE)
        return(df)
    }

    iteratedDf <- map_df(
        seq_len(4),
        ~ iterateOverISSN(df, citeScore, .x)
    ) %>%
        group_by(Title) %>%
        mutate(
            Distance = {
                ifelse(is.na(Distance), 0, Distance)
            }
        ) %>%
        top_n(1, desc(Distance)) %>%
        select(-c(Distance, Keep)) %>%
            ungroup()

    condensedDisciplines <- iteratedDf %>%
        mutate_at(vars(DataCitation:Badges), ~ case_when(
            . == 3 ~ 1,
            . == 2 ~ 1,
            . == 1 ~ 1,
            . == 0 ~ 0))                                %>%
        rename(SubjectArea = `Scopus Sub-Subject Area`) %>%
        rowwise() %>%
        mutate(Discipline = switch(
             SubjectArea,
             "Agricultural and Biological Sciences (miscellaneous)" = disc$area1,
             "Agronomy and Crop Science" = disc$area1,
             "Animal Science and Zoology" = disc$area1,
             "Aquatic Science" = disc$area1,
             "Ecology, Evolution, Behavior and Systematics" = disc$area1,
             "Food Science" = disc$area1,
             "Forestry" = disc$area1,
             "General Agricultural and Biological Sciences" = disc$area1,
             "Horticulture" = disc$area1,
             "Insect Science" = disc$area1,
             "Plant Science" = disc$area1,
             "Soil Science" = disc$area1,

             "Arts and Humanities" = disc$area2,
             "Archeology (arts and humanities)" = disc$area2,
             "Arts and Humanities (miscellaneous)" = disc$area2,
             "Classics" = disc$area2,
             "Conservation" = disc$area2,
             "General Arts and Humanities" = disc$area2,
             "History" = disc$area2,
             "History and Philosophy of Science" = disc$area2,
             "Language and Linguistics" = disc$area2,
             "Literature and Literary Theory" = disc$area2,
             "Museology" = disc$area2,
             "Music" = disc$area2,
             "Philosophy" = disc$area2,
             "Religious Studies" = disc$area2,
             "Visual Arts and Performing Arts" = disc$area2,

             "Biochemistry, Genetics and Molecular Biology" = disc$area3,
             "Aging" = disc$area3,
             "Biochemistry" = disc$area3,
             "Biochemistry, Genetics and Molecular Biology (miscellaneous)" = disc$area3,
             "Biophysics" = disc$area3,
             "Biotechnology" = disc$area3,
             "Cancer Research" = disc$area3,
             "Cell Biology" = disc$area3,
             "Clinical Biochemistry" = disc$area3,
             "Developmental Biology" = disc$area3,
             "Endocrinology" = disc$area3,
             "General Biochemistry, Genetics and Molecular Biology" = disc$area3,
             "Genetics" = disc$area3,
             "Molecular Biology" = disc$area3,
             "Molecular Medicine" = disc$area3,
             "Physiology" = disc$area3,
             "Structural Biology" = disc$area3,
             "General Biochemistry,Genetics and Molecular Biology" = disc$area3,

             "Business, Management and Accounting" = disc$area4,
             "Accounting" = disc$area4,
             "Business and International Management" = disc$area4,
             "Business, Management and Accounting (miscellaneous)" = disc$area4,
             "General Business, Management and Accounting" = disc$area4,
             "Industrial Relations" = disc$area4,
             "Management Information Systems" = disc$area4,
             "Management of Technology and Innovation" = disc$area4,
             "Marketing" = disc$area4,
             "Organizational Behavior and Human Resource Management" = disc$area4,
             "Strategy and Management" = disc$area4,
             "Tourism, Leisure and Hospitality Management" = disc$area4,
             "General Business,Management and Accounting" = disc$area4,

             "Chemical Engineering" = disc$area5,
             "Bioengineering" = disc$area5,
             "Catalysis" = disc$area5,
             "Chemical Engineering (miscellaneous)" = disc$area5,
             "Chemical Health and Safety" = disc$area5,
             "Colloid and Surface Chemistry" = disc$area5,
             "Filtration and Separation" = disc$area5,
             "Fluid Flow and Transfer Processes" = disc$area5,
             "General Chemical Engineering" = disc$area5,
             "Process Chemistry and Technology" = disc$area5,

             "Chemistry" = disc$area6,
             "Analytical Chemistry" = disc$area6,
             "Chemistry (miscellaneous)" = disc$area6,
             "Electrochemistry" = disc$area6,
             "General Chemistry" = disc$area6,
             "Inorganic Chemistry" = disc$area6,
             "Organic Chemistry" = disc$area6,
             "Physical and Theoretical Chemistry" = disc$area6,
             "Spectroscopy" = disc$area6,

             "Computer Science" = disc$area7,
             "Artificial Intelligence" = disc$area7,
             "Computational Theory and Mathematics" = disc$area7,
             "Computer Graphics and Computer-Aided Design" = disc$area7,
             "Computer Networks and Communications" = disc$area7,
             "Computer Science (miscellaneous)" = disc$area7,
             "Computer Science Applications" = disc$area7,
             "Computer Vision and Pattern Recognition" = disc$area7,
             "General Computer Science" = disc$area7,
             "Hardware and Architecture" = disc$area7,
             "Human-Computer Interaction" = disc$area7,
             "Information Systems" = disc$area7,
             "Signal Processing" = disc$area7,
             "Software" = disc$area7,

            # ! Originally this section was Decision Science
             "Decision Sciences" = disc$area8,
             "Decision Sciences (miscellaneous)" = disc$area8,
             "General Decision Sciences" = disc$area8,
             "Information Systems and Management" = disc$area8,
             "Management Science and Operations Research" = disc$area8,
             "Statistics, Probability and Uncertainty" = disc$area18,

             "Dentistry" = disc$area9,
             "Dental Assisting" = disc$area9,
             "Dental Hygiene" = disc$area9,
             "Dentistry (miscellaneous)" = disc$area9,
             "General Dentistry" = disc$area9,
             "Oral Surgery" = disc$area9,
             "Orthodontics" = disc$area9,
             "Periodontics" = disc$area9,

             "Earth and Planetary Sciences" = disc$area10,
             "Atmospheric Science" = disc$area10,
             "Computers in Earth Sciences" = disc$area10,
             "Earth and Planetary Sciences (miscellaneous)" = disc$area10,
             "Earth-Surface Processes" = disc$area10,
             "Economic Geology" = disc$area10,
             "General Earth and Planetary Sciences" = disc$area10,
             "Geochemistry and Petrology" = disc$area10,
             "Geology" = disc$area10,
             "Geophysics" = disc$area10,
             "Geotechnical Engineering and Engineering Geology" = disc$area10,
             "Oceanography" = disc$area10,
             "Paleontology" = disc$area10,
             "Space and Planetary Science" = disc$area10,
             "Stratigraphy" = disc$area10,

             "Economics, Econometrics and Finance" = disc$area11,
             "Economics and Econometrics" = disc$area11,
             "Economics, Econometrics and Finance (miscellaneous)" = disc$area11,
             "Finance" = disc$area11,
             "General Economics, Econometrics and Finance" = disc$area11,


            # ! Split this field up...?
             "Energy" = disc$area12,
             "Energy (miscellaneous)" = disc$area12,
             "Energy Engineering and Power Technology" = disc$area13,
             "Fuel Technology" = disc$area12,
             "General Energy" = disc$area12,
             "Nuclear Energy and Engineering" = disc$area13,
             "Renewable Energy, Sustainability and the Environment" = disc$area14,

             "Engineering" = disc$area13,
             "Aerospace Engineering" = disc$area13,
             "Architecture" = disc$area13,
             "Automotive Engineering" = disc$area13,
             "Biomedical Engineering" = disc$area13,
             "Building and Construction" = disc$area13,
             "Civil and Structural Engineering" = disc$area13,
             "Computational Mechanics" = disc$area13,
             "Control and Systems Engineering" = disc$area13,
             "Electrical and Electronic Engineering" = disc$area13,
             "Engineering (miscellaneous)" = disc$area13,
             "General Engineering" = disc$area13,
             "Industrial and Manufacturing Engineering" = disc$area13,
             "Mechanical Engineering" = disc$area13,
             "Mechanics of Materials" = disc$area13,
             "Media Technology" = disc$area13,
             "Ocean Engineering" = disc$area13,
             "Safety, Risk, Reliability and Quality" = disc$area13,

             "Environmental Science" = disc$area14,
             "Ecological Modeling" = disc$area14,
             "Ecology" = disc$area14,
             "Environmental Chemistry" = disc$area14,
             "Environmental Engineering" = disc$area14,
             "Environmental Science (miscellaneous)" = disc$area14,
             "General Environmental Science" = disc$area14,
             "Global and Planetary Change" = disc$area14,
             "Health, Toxicology and Mutagenesis" = disc$area14,
             "Management, Monitoring, Policy and Law" = disc$area14,
             "Nature and Landscape Conservation" = disc$area14,
             "Pollution" = disc$area14,
             "Waste Management and Disposal" = disc$area14,
             "Water Science and Technology" = disc$area14,

             "Health Professions" = disc$area15,
             "Chiropractics" = disc$area15,
             "Complementary and Manual Therapy" = disc$area15,
             "Emergency Medical Services" = disc$area15,
             "General Health Professions" = disc$area15,
             "Health Information Management" = disc$area15,
             "Health Professions (miscellaneous)" = disc$area15,
             "Medical Assisting and Transcription" = disc$area15,
             "Medical Laboratory Technology" = disc$area15,
             "Medical Terminology" = disc$area15,
             "Occupational Therapy" = disc$area15,
             "Optometry" = disc$area15,
             "Pharmacy" = disc$area15,
             "Physical Therapy, Sports Therapy and Rehabilitation" = disc$area15,
             "Podiatry" = disc$area15,
             "Radiological and Ultrasound Technology" = disc$area15,
             "Respiratory Care" = disc$area15,
             "Speech and Hearing" = disc$area15,

             "Immunology and Microbiology" = disc$area16,
             "Applied Microbiology and Biotechnology" = disc$area16,
             "General Immunology and Microbiology" = disc$area16,
             "Immunology" = disc$area16,
             "Immunology and Microbiology (miscellaneous) " = disc$area16,
             "Microbiology" = disc$area16,
             "Parasitology" = disc$area16,
             "Virology" = disc$area16,

             "Materials Science" = disc$area17,
             "Biomaterials" = disc$area17,
             "Ceramics and Composites" = disc$area17,
             "Electronic, Optical and Magnetic Materials" = disc$area17,
             "General Materials Science" = disc$area17,
             "Materials Chemistry" = disc$area17,
             "Materials Science (miscellaneous)" = disc$area17,
             "Metals and Alloys" = disc$area17,
             "Polymers and Plastics" = disc$area17,
             "Surfaces, Coatings and Films" = disc$area17,

             "Mathematics" = disc$area18,
             "Algebra and Number Theory" = disc$area18,
             "Analysis" = disc$area18,
             "Applied Mathematics" = disc$area18,
             "Computational Mathematics" = disc$area18,
             "Control and Optimization" = disc$area18,
             "Discrete Mathematics and Combinatorics" = disc$area18,
             "General Mathematics" = disc$area18,
             "Geometry and Topology" = disc$area18,
             "Logic" = disc$area18,
             "Mathematical Physics" = disc$area18,
             "Mathematics (miscellaneous)" = disc$area18,
             "Modeling and Simulation" = disc$area18,
             "Numerical Analysis" = disc$area18,
             "Statistics and Probability" = disc$area18,
             "Theoretical Computer Science" = disc$area18,

             "Medicine" = disc$area19,
             "Anatomy" = disc$area19,
             "Anesthesiology and Pain Medicine" = disc$area19,
             "Biochemistry (medical)" = disc$area19,
             "Cardiology and Cardiovascular Medicine" = disc$area19,
             "Complementary and Alternative Medicine" = disc$area19,
             "Critical Care and Intensive Care Medicine" = disc$area19,
             "Dermatology" = disc$area19,
             "Drug Guides" = disc$area19,
             "Embryology" = disc$area19,
             "Emergency Medicine" = disc$area19,
             "Endocrinology, Diabetes and Metabolism" = disc$area19,
             "Epidemiology" = disc$area19,
             "Family Practice" = disc$area19,
             "Gastroenterology" = disc$area19,
             "General Medicine" = disc$area19,
             "Genetics (clinical)" = disc$area19,
             "Geriatrics and Gerontology" = disc$area19,
             "Health Informatics" = disc$area19,
             "Health Policy" = disc$area19,
             "Hematology" = disc$area19,
             "Hepatology" = disc$area19,
             "Histology" = disc$area19,
             "Immunology and Allergy" = disc$area19,
             "Infectious Diseases" = disc$area19,
             "Internal Medicine" = disc$area19,
             "Medicine (miscellaneous)" = disc$area19,
             "Microbiology (medical)" = disc$area19,
             "Nephrology" = disc$area19,
             "Neurology (clinical)" = disc$area19,
             "Obstetrics and Gynecology" = disc$area19,
             "Oncology" = disc$area19,
             "Ophthalmology" = disc$area19,
             "Orthopedics and Sports Medicine" = disc$area19,
             "Otorhinolaryngology" = disc$area19,
             "Pathology and Forensic Medicine" = disc$area19,
             "Pediatrics, Perinatology and Child Health" = disc$area19,
             "Pharmacology (medical)" = disc$area19,
             "Physiology (medical)" = disc$area19,
             "Psychiatry and Mental Health" = disc$area19,
             "Public Health, Environmental and Occupational Health" = disc$area19,
             "Pulmonary and Respiratory Medicine" = disc$area19,
             "Radiology, Nuclear Medicine and Imaging" = disc$area19,
             "Rehabilitation" = disc$area19,
             "Reproductive Medicine" = disc$area19,
             "Reviews and References (medical)" = disc$area19,
             "Rheumatology" = disc$area19,
             "Surgery" = disc$area19,
             "Transplantation" = disc$area19,
             "Urology" = disc$area19,

             " " = disc$area20,
             "Behavioral Neuroscience" = disc$area20,
             "Biological Psychiatry" = disc$area20,
             "Cellular and Molecular Neuroscience" = disc$area20,
             "Cognitive Neuroscience" = disc$area20,
             "Developmental Neuroscience" = disc$area20,
             "Endocrine and Autonomic Systems" = disc$area20,
             "General Neuroscience" = disc$area20,
             "Neurology" = disc$area20,
             "Neuroscience (miscellaneous)" = disc$area20,
             "Sensory Systems" = disc$area20,

             "Nursing" = disc$area21,
             "Advanced and Specialized Nursing" = disc$area21,
             "Assessment and Diagnosis" = disc$area21,
             "Care Planning" = disc$area21,
             "Community and Home Care" = disc$area21,
             "Critical Care Nursing" = disc$area21,
             "Emergency Nursing" = disc$area21,
             "Fundamentals and Skills" = disc$area21,
             "General Nursing" = disc$area21,
             "Gerontology" = disc$area21,
             "Issues, Ethics and Legal Aspects" = disc$area21,
             "LPN and LVN" = disc$area21,
             "Leadership and Management" = disc$area21,
             "Maternity and Midwifery" = disc$area21,
             "Medical and Surgical Nursing" = disc$area21,
             "Nurse Assisting" = disc$area21,
             "Nursing (miscellaneous)" = disc$area21,
             "Nutrition and Dietetics" = disc$area21,
             "Oncology (nursing)" = disc$area21,
             "Pathophysiology" = disc$area21,
             "Pediatrics" = disc$area21,
             "Pharmacology (nursing)" = disc$area21,
             "Psychiatric Mental Health" = disc$area21,
             "Research and Theory" = disc$area21,
             "Review and Exam Preparation" = disc$area21,

             "Pharmacology, Toxicology and Pharmaceutics" = disc$area22,
             "Drug Discovery" = disc$area22,
             "General Pharmacology, Toxicology and Pharmaceutics" = disc$area22,
             "Pharmaceutical Science" = disc$area22,
             "Pharmacology" = disc$area22,
             "Pharmacology, Toxicology and Pharmaceutics (miscellaneous)" = disc$area22,
             "Toxicology" = disc$area22,

             "Physics and Astronomy" = disc$area23,
             "Acoustics and Ultrasonics" = disc$area23,
             "Astronomy and Astrophysics" = disc$area23,
             "Atomic and Molecular Physics, and Optics" = disc$area23,
             "Condensed Matter Physics" = disc$area23,
             "General Physics and Astronomy" = disc$area23,
             "Instrumentation" = disc$area23,
             "Nuclear and High Energy Physics" = disc$area23,
             "Physics and Astronomy (miscellaneous)" = disc$area23,
             "Radiation" = disc$area23,
             "Statistical and Nonlinear Physics" = disc$area23,
             "Surfaces and Interfaces" = disc$area23,

             "Psychology" = disc$area24,
             "Applied Psychology" = disc$area24,
             "Clinical Psychology" = disc$area24,
             "Developmental and Educational Psychology" = disc$area24,
             "Experimental and Cognitive Psychology" = disc$area24,
             "General Psychology" = disc$area24,
             "Neuropsychology and Physiological Psychology" = disc$area24,
             "Psychology (miscellaneous)" = disc$area24,
             "Social Psychology" = disc$area24,

             "Social Sciences" = disc$area25,
             "Anthropology" = disc$area25,
             "Archeology" = disc$area25,
             "Communication" = disc$area25,
             "Cultural Studies" = disc$area25,
             "Demography" = disc$area25,
             "Development" = disc$area25,
             "Education" = disc$area25,
             "Gender Studies" = disc$area25,
             "General Social Sciences" = disc$area25,
             "Geography, Planning and Development" = disc$area25,
             "Health (social science)" = disc$area25,
             "Human Factors and Ergonomics" = disc$area25,
             "Library and Information Sciences" = disc$area25,
             "Life-span and Life-course Studies" = disc$area25,
             "Linguistics and Language" = disc$area25,
             "Political Science and International Relations" = disc$area25,
             "Public Administration" = disc$area25,
             "Safety Research" = disc$area25,
             "Social Sciences (miscellaneous)" = disc$area25,
             "Sociology and Political Science" = disc$area25,
             "Transportation" = disc$area25,
             "Urban Studies" = disc$area25,

             "Veterinary" = disc$area26,
             "Equine" = disc$area26,
             "Food Animals" = disc$area26,
             "General Veterinary" = disc$area26,
             "Small Animals" = disc$area26,
             "Veterinary (miscellaneous)" = disc$area26,

             "Law" = disc$area27,

            "Other"
        )) %>%
        mutate(GroupedDisc = switch(
            Discipline,

            # Math, Chem, Enviro, & Bio Sciences
            "Agricultural and Biological Sciences" = grouped_disc$area1,
            "Biochemistry, Genetics and Molecular Biology" = grouped_disc$area1,
            "Biochemistry,Genetics and Molecular Biology" = grouped_disc$area1,
            "Environmental Science" = grouped_disc$area1,
            "Immunology and Microbiology" = grouped_disc$area1,

            # Tech & Comp Sciences
            "Computer Science" = grouped_disc$area2,
            "Mathematics" = grouped_disc$area2,

            # Engineering
            "Engineering" = grouped_disc$area3,
            "Chemical Engineering" = grouped_disc$area3,

            # Medical & Health Sciences
            "Medicine" = grouped_disc$area4,
            "Health Professions" = grouped_disc$area4,
            "Nursing" = grouped_disc$area4,
            "Pharmacology, Toxicology and Pharmaceutics" = grouped_disc$area4,
            "Veterinary" = grouped_disc$area4,

            # ASSH
            "Arts and Humanities" = grouped_disc$area5,
            "Social Sciences" = grouped_disc$area5,

            # Business & Law
            "Business, Management and Accounting" = grouped_disc$area6,
            "Economics, Econometrics and Finance" = grouped_disc$area6,
            "Decision Science" = grouped_disc$area6,
            "Law" = grouped_disc$area6,

            # Psych & Cog Sciences
            "Psychology" = grouped_disc$area7,
            "Neuroscience" = grouped_disc$area7,

            # Physical Sciences
            "Earth and Planetary Sciences" = grouped_disc$area8,
            "Physics and Astronomy" = grouped_disc$area8,
            "Chemistry" = grouped_disc$area8,

            "Other"
        )) %>%
            ungroup() %>%
            group_by(GroupedDisc) %>%
            distinct(Title, .keep_all = TRUE) %>%
            add_tally()

    return(condensedDisciplines)
}
