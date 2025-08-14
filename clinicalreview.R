# ---- 0. Load score rule ----
source("R/configure.R")
library(AutoAnnotation)
library(dplyr)
library(rsyrf)
source("R/getAnnotationsFunctionsV2.R")
source("R/calculateMSDrugScores.R")
source('R/dictionaryValidationHelpers.R')


#load data
MSAnnotations <- read.csv('data/ClinicalMSAnnotations.csv')
MSStudies <- read.csv('data/ClinicalMSStudies.csv')
includedOutcomes <- read.csv('data/clinicalMSIncludedOutcomes.csv')
scoreRulesRaw <-read.csv('data/allScoreRules.csv')
allScoreRules <- list(scoreRules1 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 1, ]), scoreRules2 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 2, ]), scoreRules3 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 3, ]))
scoreRule <- allScoreRules$scoreRules3


# ---- Here set up the investigators interested ----

# 
# thisInvestigatorIdStrs <- sapply(rsyrf::getInvestigatorIdsForProjectId(projectIds = clinicalProjectId, idtype = idtype), rsyrf::convertRawIdToUUID)
# 
# jiaruiInvestigatorIdStr <- '75580d01-64a8-407a-a481-fcf832b23e08'
# OctopusInvestigatorIdStr<- jiaruiInvestigatorIdStr
# 
# thisStageIdStrs <- MSDataExtractionStageIdStr
# 
# # ---- 1. Load data ----
# # Load score rules
# scoreRulesRaw <- googlesheets4::read_sheet(googleSheetId, sheet = scoreRuleSheetName, trim_ws = F)
# allScoreRules <- list(scoreRules1 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 1, ]), scoreRules2 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 2, ]), scoreRules3 = getScoreRuleData(scoreRulesRaw[scoreRulesRaw$ScoreRuleVersion == 3, ]))
# scoreRule <- allScoreRules$scoreRules3
# 
# #Load clinical project
# clinicalProject <-  rsyrf::getProjects(projectIds = clinicalProjectId, idtype = idtype)
# 
# # stage depends on thisStageIdStrs
# clinicalStages <-  rsyrf::getStagesForProject(projects = clinicalProject) %>% filter(idStr %in% thisStageIdStrs)
# 
# stageQuestionIdStrs <- unique(unlist(sapply(clinicalStages$AnnotationQuestions, function(x){  sapply(x,  rsyrf::convertRawIdToUUID)})))
# 
# MSClinicalSessions <- getAnnotationSessionsForIds2(projectIds = rsyrf::convertIdBase64ToUUID(clinicalProjectId), investigatorIds = thisInvestigatorIdStrs,  stageIds = thisStageIdStrs)
# 
# MSClinicalSessions <- MSClinicalSessions %>%
#   filter( Status == 1 &
#             InvestigatorIdStr %in% OctopusInvestigatorIdStr &
#             StageIdStr %in% clinicalStages$idStr) %>%
#   group_by(InvestigatorIdStr, StageIdStr, StudyIdStr, Status) %>%
#   summarise(
#     count = n()
#   )
# 
# MSStudies <- getStudies(projectIds = rsyrf::convertIdBase64ToUUID(clinicalProjectId), studyIds = unique(MSClinicalSessions$StudyIdStr))
# 
# investigators <- rsyrf::getInvestigators(investigatorIds = thisInvestigatorIdStrs)
# 
# MSAnnotations <- getAnnotationsFull2(studies = MSStudies, investigators = investigators, completeSessions = T) %>%
#   # filter(AnnotatorIdStr %in% thisInvestigatorIdStrs & QuestionIdStr %in% stageQuestionIdStrs) %>%
#   filter(AnnotatorIdStr %in% thisInvestigatorIdStrs) %>%
#   left_join(MSClinicalSessions, by = c("StudyIdStr" = "StudyIdStr",  "AnnotatorIdStr" = "InvestigatorIdStr")) %>%
#   filter(!is.na(StageIdStr))
# 
# MSAnnotations <- apply(MSAnnotations,2,as.character)
# 
# # write out annotation data
# 
# write.csv(MSAnnotations, 'jiarui/ClinicalMSAnnotations1.csv', row.names = FALSE)
# 
# MSAnnotations <- read.csv('data/ClinicalMSAnnotations1.csv')

# ANALYSIS ----

includedMSAnnotations <-  MSAnnotations %>% filter(!StudyIdStr %in% unique(MSAnnotations[MSAnnotations$QuestionIdStr == MSExclusionIdStr & !is.na(MSAnnotations$Answer), "StudyIdStr"]))
includedMSAnnotations <- rsyrf::cleanAnnotations(includedMSAnnotations)
# rm(MSAnnotations)
includedMSAnnotations$uniqueIdStr <- paste(includedMSAnnotations$StudyIdStr, includedMSAnnotations$AnnotatorIdStr, includedMSAnnotations$StageIdStr, sep = "_")

includedMSStudies <- MSStudies[MSStudies$idStr %in% unique(includedMSAnnotations$StudyIdStr),]
# rm(MSStudies)


# includedOutcomes <- rsyrf::cleanOutcomes(getOutcomeForStudies(studies = includedMSStudies))



includedOutcomes$uniqueIdStr <- paste(includedOutcomes$StudyIdStr, includedOutcomes$AnnotatorIdStr, includedOutcomes$StageIdStr, sep = "_")

uniqueIdStrs <- includedMSAnnotations %>% 
  select(StageIdStr, AnnotatorIdStr, Annotator, StudyIdStr) %>%
  unique() %>%
  group_by(AnnotatorIdStr, Annotator, StudyIdStr, StageIdStr) %>%
  mutate(
    uniqueIdStr = paste(StudyIdStr, AnnotatorIdStr, StageIdStr, sep = "_")
  )

#----- 2. Calculate scores for each intervention in each study ----
# allIndex <- which(uniqueIdStrs$StageIdStr == dataExtractionStageIdStrs$dataExtractionStage2)
allIndex <- seq_along(uniqueIdStrs$uniqueIdStr)
###

for(index in allIndex) {
  # print(paste("----------",index," ----------"))
  iuniqueIdStr <- uniqueIdStrs$uniqueIdStr[index]
  #  iuniqueIdStr <- "f3532c1f-4a52-4239-a03d-b18722508e97_beef0b04-01c2-402f-9da5-b414b773b4d3_2c400348-d871-4055-9c68-2bc529ac9ccc"
  annotationsPerSession <- includedMSAnnotations %>% filter(uniqueIdStr == iuniqueIdStr)
  outcomesPerSession <- includedOutcomes %>% filter(uniqueIdStr == iuniqueIdStr)
  
  scoresPerIntervention <- calculateMSDrugScorePerIntervention(annotationsPerSession =  annotationsPerSession, outcomesPerSession = outcomesPerSession,  dataExtractionStageIdStrs =  dataExtractionStageIdStrs, allScoreRules = allScoreRules)
  
  if(!is.null(scoresPerIntervention))  if(!exists('fullScoresPerIntervention')) fullScoresPerIntervention <- scoresPerIntervention else fullScoresPerIntervention <- rbind(fullScoresPerIntervention, scoresPerIntervention)
}


for(icol in 1:ncol(fullScoresPerIntervention)) {
  if(class(fullScoresPerIntervention[,icol, drop = TRUE])[[1]]=="numeric" ) {
    fullScoresPerIntervention[is.infinite(fullScoresPerIntervention[,icol, drop = TRUE]) | is.na(fullScoresPerIntervention[,icol, drop = TRUE]), icol] <- NA
  }
}

fullScoresPerIntervention<-calculateWeight(fullScoresPerIntervention)
fullScoresPerIntervention<-calculateDistanceScore(fullScoresPerIntervention)

# ---- 3. use regex to match Drug name in fullScoresPerIntervention to drugBank names
drugNamesSheetId <- "1J_Sns880m4rh7JsLdKu0STdUnlEayUDhYCTy6pGAzJM" 
drugBankDictionary <- googlesheets4::read_sheet(drugNamesSheetId, sheet="drugBankDictionary")  
drugVocabulary<-googlesheets4::read_sheet(drugNamesSheetId, sheet="drugVocabulary", col_types = c("ccccccc"))  

myDrug <- as.data.frame(unique(fullScoresPerIntervention$Drug))
names(myDrug) <- "Name"
myDrug$id <- seq_along(myDrug$Name)

minimumIncludeFrequencies <- 6
maximumExcludeFrequencies <- 1

results1 <- ExtractDrug(myDrug, dictionaryName =  as.data.frame(drugBankDictionary), idColumn = "id", textSearchingHeaders = c("Name"), minimumIncludeFrequency = 6,  maximumExcludeFrequency = 1, 
                        #varnames = c("Drug", "DrugFrequency"), 
                        groupAnnotation=F)
#, ignoreCase=T)

MSDrugList<-merge(myDrug, results1) %>% 
  mutate(Drug = ifelse(is.na(Drug), Name, Drug)) %>% 
  select(Drug, Name) %>%
  rename(RegexDrug = Drug, Drug = Name)

fullScoresPerIntervention1 <- left_join(fullScoresPerIntervention, MSDrugList, by = "Drug")
fullScoresPerIntervention1 <- fullScoresPerIntervention1%>%
  mutate(Drug = RegexDrug)%>%
  select(-RegexDrug)

validIndex <- which((!is.na(fullScoresPerIntervention1$efficacy_scores) | !is.na(fullScoresPerIntervention1$saftey_scores)) & !is.na(fullScoresPerIntervention1$quality_scores) & !is.na(fullScoresPerIntervention1$studySize_scores))

invalidScores <- fullScoresPerIntervention1[-validIndex,]

validScores <- fullScoresPerIntervention1[validIndex,]

fullScoresPerIntervention1$valid <- ifelse((!is.na(fullScoresPerIntervention1$efficacy_scores) | !is.na(fullScoresPerIntervention1$saftey_scores)) & !is.na(fullScoresPerIntervention1$quality_scores) & !is.na(fullScoresPerIntervention1$nParticipants), T, F )


# ---- 4. write out reconciled + first annotated data ----

MSreconciledAndFirstAnnotatedScores <- MSreconciledAndFirstAnnotatedSessions[,c("StudyIdStr","chosenAnnotator")] %>%
  left_join(validScores, by = c("StudyIdStr" = "StudyIdStr", "chosenAnnotator" = "Annotator")  ) %>%
  select("StudyIdStr", "Drug", "Annotator"= "chosenAnnotator", "AnnotatorIdStr", "ExperimentIdStr", 
         #"StageIdStr", 
         "Disease", "MSType", "StudyType","Phase", "ObservationalStudyType", "CrossOverStudy","nParticipants","efficacy_scores" , "saftey_scores", #"studySize_scores",
         "quality_scores",
         "weight",
         "distanceScores")
#"product_scores")

MSreconciledAndFirstAnnotatedPublicationList <- MSreconciledAndFirstAnnotatedScores[, c("StudyIdStr","Drug","Disease", "MSType", "nParticipants","efficacy_scores","saftey_scores","quality_scores", "weight", "distanceScores",
                                                                                        #"studySize_scores","product_scores",
                                                                                        "StudyType","Phase", "ObservationalStudyType")] %>%
  left_join(includedMSStudies[, c("idStr", "Title", "Abstract", "DOI", "Year", "Journal", "Author","PdfRelativePath","SystematicSearchIdStr")], by = c("StudyIdStr" = "idStr")) %>%
  mutate(
    PdfRelativePath = ifelse(!is.na(PdfRelativePath) & PdfRelativePath != "", paste("https://ecrf1.clinicaltrials.ed.ac.uk/camarades", SystematicSearchIdStr, PdfRelativePath,sep="/"), "")
  ) %>%
  select("StudyIdStr", "Disease", "MSType", "Drug", "Title", "Abstract", "DOI", "Year", "Author", "Journal", "PdfRelativePath", "studyType" = "StudyType",    "phase"= "Phase", "observationalStudyType" = "ObservationalStudyType",   "nPatients" = "nParticipants",    "efficacyScores" = "efficacy_scores",    "safteyScores" = "saftey_scores",
         # "studySizeScore" = "studySize_scores",
         "qualityScore" = "quality_scores" ,
         "weight",
         #"product_scores" = "product_scores"
         "distanceScore" = "distanceScores") %>%
  unique()

MSreconciledAndFirstAnnotatedPublicationList$Drug <- tolower(MSreconciledAndFirstAnnotatedPublicationList$Drug)

write.csv(MSreconciledAndFirstAnnotatedPublicationList, 'jiarui/ClinicalMSPublicationList.csv')



MSreconciledAndFirstAnnotatedDrugSummary <- calculateMSDrugSummary(MSreconciledAndFirstAnnotatedScores)%>%
  filter(is.na(Drug) == FALSE)

write.csv(MSreconciledAndFirstAnnotatedDrugSummary, 'jiarui/ClinicalMSDrugSummary.csv')


MSreconciledAndFirstAnnotatedDrugList <- data.frame(drugList = sort(unique(trimws(MSreconciledAndFirstAnnotatedDrugSummary$Drug))))

write.csv(MSreconciledAndFirstAnnotatedDrugList, 'jiarui/ClinicalMSDrugList.csv')

# 
# 
# ----5. write out progress summary

#get full relisyr included study list from mySQL; then select those identified by regex as progressiveMS studies
# clinicalStudyList <- RMySQL::dbReadTable(mySQLCon, clinicalMySQLTableName)

# progressiveMSPublications <- clinicalStudyList%>%filter(MSType == "Progressive MS")%>%
  # group_by(Title, Author, Journal, Abstract, Year, OldIdStr, HistoricalID, PublicationID, Disease, Link, idStr, ProjectIdStr, MSType)%>%
  # summarise(Drug = paste(Drug, collapse = ";"))%>%
  # rename(StudyIdStr = idStr)

# 
# 
# progressiveMSStudyIdStr <- clinicalStudyList%>%filter(MSType == "Progressive MS")%>%select(NewIdStr)%>%
#   rename(StudyIdStr = NewIdStr)%>%unique()

#get annotations sessions per study to look at single annotated/dual annotated/reconciled
clinicalAnnotationSessionsPerStudy <- MSClinicalSessions %>%
  group_by(StudyIdStr) %>%
  summarise(
    nReviewers = length(unique(InvestigatorIdStr)),
    hasReconciler = any(InvestigatorIdStr %in% reconcilerIdStrs)
  )

includedMSStudyIdStr <- MSreconciledAndFirstAnnotatedPublicationList%>%select(StudyIdStr)



excludedMSAnnotations<- MSAnnotations %>% 
  filter(!StudyIdStr %in% includedMSStudyIdStr$StudyIdStr)

excludedPapers <- excludedMSAnnotations %>% filter (QuestionIdStr == MSExclusionIdStr ) %>% select(StudyIdStr,Title, Answer)%>%
  filter(Answer != "does not describe disease of interest")%>%
  rename(excludedReason = Answer)

excludedPapers <- excludedPapers[!duplicated(excludedPapers$StudyIdStr),]

progressiveMSStudyIdStr <-progressiveMSStudyIdStr%>%
  mutate(inclusionStatus = ifelse(StudyIdStr %in% includedMSStudyIdStr$StudyIdStr, "include", ifelse(StudyIdStr %in% excludedPapers$StudyIdStr, "exclude", "pending")),
         source = "progressiveMS")

progressiveMSStudyIdStr <- left_join(progressiveMSStudyIdStr, 
                                     excludedPapers[, c("StudyIdStr", "excludedReason")], by = "StudyIdStr")

progressiveMSStudies <- left_join(progressiveMSStudyIdStr, clinicalAnnotationSessionsPerStudy, by = "StudyIdStr")

otherIncludedMSpapers <- MSreconciledAndFirstAnnotatedPublicationList%>%
  filter(!StudyIdStr %in% progressiveMSStudyIdStr$StudyIdStr)%>%
  select(StudyIdStr)%>%
  unique()%>%
  mutate(inclusionStatus = "include", excludedReason = "NULL", source = "relisyrlonglist")

otherIncludedMSpapers <- left_join(otherIncludedMSpapers, clinicalAnnotationSessionsPerStudy, by= "StudyIdStr")

allMSpapers <- rbind(progressiveMSStudies, otherIncludedMSpapers)
is.na(allMSpapers) <- allMSpapers == "NULL"

write.csv(allMSpapers, 'jiarui/MSClincalInclusionSummary.csv')


# allAnnotatedMSpapers <- allMSpapers%>%filter(inclusionStatus != "pending")

# inclusionSummary<-allMSpapers%>%
#   group_by(inclusionStatus, excludedReason, source, nReviewers, hasReconciler)%>%
#   summarise(nStudies = length(unique(StudyIdStr)))

entityOfInterest <- googlesheets4::read_sheet(googleSheetId, sheet = entityOfInterestSheetName)
diseaseOfInterest  <- entityOfInterest[entityOfInterest$Type == "diseaseOfInterest", "Item"]$Item
drugOfInterest  <- entityOfInterest[entityOfInterest$Type == "drugOfInterest", "Item"]$Item
crossTable <- as.data.frame.matrix(table(clinicalStudyList[,c("Drug","Disease")]))

chosenDiseases <- "MND"

crossTable$select  <- F

for(chosenDiease in chosenDiseases){
  crossTable$score1 <- rowSums(crossTable[, chosenDiease, drop = F])
  crossTable$score2 <- rowSums(crossTable[, setdiff(diseaseOfInterest, chosenDiease), drop=F] > 0)
  crossTable$select <- crossTable$select | (crossTable$score1 > 0 | crossTable$score2 >= 2)
}
crossTable <- crossTable[which(crossTable$select), ]

clinicalDrugMeetLogic <- rownames(crossTable[which(crossTable$select), ])
clinicalPublicationsMeetLogic <- clinicalStudyList[which(clinicalStudyList$Drug %in%  clinicalDrugMeetLogic),]

clinicalCoreDrugs <- intersect(drugOfInterest, clinicalDrugMeetLogic)
clinicalPublicationsCoreDrugs <- clinicalStudyList[which(clinicalStudyList$Drug %in%  clinicalCoreDrugs),]

clinicalProgressSummary <- data.frame(
  ReviewType = "Clinical",
  nUniquePublications =  39753,
  nIncludedPublications = 11236,
  # nDrugMeetLogic =  length(clinicalDrugMeetLogic),
  # nPublicationsMeetLogic = length(unique(clinicalPublicationsMeetLogic$NewIdStr)),
  # longListLatestUpdatedDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000Z"),
  # nCoreDrugs = length(clinicalCoreDrugs),
  # nCoreDrugPublications = length(unique(clinicalPublicationsCoreDrugs$NewIdStr)),
  # nCoreDrugMSPublications = length(unique(otherIncludedMSpapers$StudyIdStr)),
  nCoreMSSingleAnnotated = sum(otherIncludedMSpapers$nReviewers == 1),
  nCoreMSDualAnnotated = sum(otherIncludedMSpapers$nReviewers > 1),
  nCoreMSReconciled = sum(otherIncludedMSpapers$hasReconciler & otherIncludedMSpapers$nReviewers > 2),
  nProgressiveMSPublications = nrow(unique(filter(allMSpapers, source == "progressiveMS"))),
  nProgressiveMSIncludedPublications = nrow(unique(filter(allMSpapers, source == "progressiveMS", inclusionStatus == "include"))),
  nProgressiveMSExcludedPublications = nrow(unique(filter(allMSpapers, source == "progressiveMS", inclusionStatus == "exclude"))),
  nProgressiveSingleAnnotated = sum(na.omit(filter(progressiveMSStudies, inclusionStatus != "pending")$nReviewers) == 1),
  nProgressiveDualAnnotated = sum(na.omit(filter(progressiveMSStudies, inclusionStatus != "pending")$nReviewers) > 1),
  nProgressiveReconciled = sum(na.omit(filter(progressiveMSStudies, inclusionStatus != "pending")$nReviewers) > 2 &
                                 na.omit(filter(progressiveMSStudies, inclusionStatus != "pending")$hasReconciler)),
  nFTExcluded = nrow(unique(filter(allMSpapers, inclusionStatus =="exclude"))),
  nFTIncluded = nrow(unique(filter(allMSpapers, inclusionStatus == "include"))),
  ReviewLatestUpdatedDate =  max(as.Date(MSAnnotations$DateTimeCreated))
)

write.csv(clinicalProgressSummary, 'data/ClinicalMSProgressSummary.csv')

