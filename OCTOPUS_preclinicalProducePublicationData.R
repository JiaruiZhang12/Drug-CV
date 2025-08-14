source("R/configure.R")
library(rsyrf)

#-----1. Get relevant annotation and session data

#Load invivo project
invivoProject <-  rsyrf::getProjects(projectIds = invivoProjectId, idtype = idtype)

# get Stages for project
thisStageIdStrs <- invivoDataExtractionStageIdStr
invivoStages <-  rsyrf::getStagesForProject(projects = invivoProject) %>% filter(idStr %in% thisStageIdStrs)

stageQuestionIdStrs <- unique(unlist(sapply(invivoStages$AnnotationQuestions, function(x){  sapply(x,  rsyrf::convertRawIdToUUID)})))

#get investigator Id and investigator data for invivo project
thisInvestigatorIdStrs <- sapply(rsyrf::getInvestigatorIdsForProjectId(projectIds = invivoProjectId, idtype = idtype), rsyrf::convertRawIdToUUID)
investigators <- rsyrf::getInvestigators(investigatorIds = thisInvestigatorIdStrs)

#get annotation session data using project, stage, and investigator ids
invivoSessions <- getAnnotationSessionsForIds2(projectIds = rsyrf::convertIdBase64ToUUID(invivoProjectId), investigatorIds = thisInvestigatorIdStrs,  stageIds = invivoStages$idStr)

invivoSessions <- invivoSessions %>% 
  filter(Status == 1 & InvestigatorIdStr %in% thisInvestigatorIdStrs & StageIdStr %in% invivoStages$idStr) %>%
  group_by(InvestigatorIdStr, StageIdStr, StudyIdStr, Status) %>%
  summarise(
    count = n()
  )

#get studies using project, study Ids from sessions
invivoStudies <- rsyrf::getStudies(projectIds = rsyrf::convertIdBase64ToUUID(invivoProjectId), studyIds = unique(invivoSessions$StudyIdStr))

#get all annotations and included annotations data, outcomes, studies using study data, investigator data and filter by complete sessions
invivoAnnotations <- getAnnotationsFull2(studies = invivoStudies, investigators = investigators) %>%
  filter(AnnotatorIdStr %in% thisInvestigatorIdStrs) %>%
  left_join(invivoSessions, by = c("StudyIdStr" = "StudyIdStr",  "AnnotatorIdStr" = "InvestigatorIdStr")) %>%
  filter(!is.na(StageIdStr))




# invivoAnnotations$Notes <- as.character(invivoAnnotations$Notes)
# invivoAnnotations$Answer <- as.character(invivoAnnotations$Answer)
# 
# invivoAnnotations <- invivoAnnotations%>%
#   mutate(Answer = ifelse(Notes != "" & Answer == "other (free text)", Notes, Answer))
# 
# invivoAnnotations$Answer <- as.factor(invivoAnnotations$Answer)

#find questionIdStr for FT exclusion
invivoExclusionQuestion <- invivoAnnotations%>% 
  select(Question,
         QuestionIdStr) %>%
  unique()%>%
  filter(Question == 'I would like to exclude this publication because: ')

invivoExclusionIdStr <- invivoExclusionQuestion$QuestionIdStr

includedinvivoAnnotations <- invivoAnnotations %>% filter(!StudyIdStr %in% unique(invivoAnnotations[invivoAnnotations$QuestionIdStr == invivoExclusionIdStr & !is.na(invivoAnnotations$Answer), "StudyIdStr"]))
includedinvivoAnnotations <- rsyrf::cleanAnnotations(includedinvivoAnnotations)
# rm(invivoAnnotations)
includedinvivoAnnotations$uniqueIdStr <- paste(includedinvivoAnnotations$StudyIdStr, includedinvivoAnnotations$AnnotatorIdStr, includedinvivoAnnotations$StageIdStr, sep = "_")


includedStudies <- invivoStudies[invivoStudies$idStr %in% unique(includedinvivoAnnotations$StudyIdStr),]
# rm(invivoStudies)

# includedOutcomes <-rsyrf:::cleanOutcomes(rsyrf::getOutcomeForStudies(studies = includedStudies))

includedOutcomes <-cleanOutcomes(getOutcomeForStudies2(studies = includedStudies))

includedOutcomes$uniqueIdStr <- paste(includedOutcomes$StudyIdStr, includedOutcomes$InvestigatorIdStr, includedOutcomes$StageIdStr, sep = "_")

uniqueIdStrs <- includedinvivoAnnotations %>% 
  select(StageIdStr, AnnotatorIdStr, Annotator, StudyIdStr) %>%
  unique() %>%
  group_by(AnnotatorIdStr, Annotator, StudyIdStr) %>%
  mutate(
    uniqueIdStr = paste(StudyIdStr, AnnotatorIdStr, StageIdStr, sep = "_")
  )

#-----2. Get questions from project and set question order / label 
invivoQuestions<-getQuestionsForProject(invivoProject)
orderQuestions<- invivoQuestions%>%
  select(Question, Category, Subquestions, idStr)

colnames(orderQuestions)[colnames(orderQuestions) == "idStr"] = "QuestionIdStr"
colnames(orderQuestions)[colnames(orderQuestions) == "Subquestions"] = "subquestionIds"

orderQuestions<-addIdStrs(orderQuestions)

parentId<-NULL
for (i in orderQuestions$QuestionIdStr){
  parent_id<-orderQuestions$QuestionIdStr[str_which(orderQuestions$subquestionIdStrs, i)]
  if(length(parent_id) == 0 ){
    parentId[i] = NA
  }
  
  else{ 
    parentId[i] = parent_id
    
  }
  
  parentId
  
}

df<-data.frame(parentId, QuestionIdStr = names(parentId))
orderQuestions<-left_join(orderQuestions,df, by = "QuestionIdStr")%>%select(-subquestionIdStrs)

annotationQuestions<-invivoAnnotations%>%
  select(QuestionIdStr, Question)%>%
  unique()

orderQuestions<-left_join(annotationQuestions, orderQuestions, by = c("QuestionIdStr", "Question"))

#write csv, then within excel add 
#order = order of question; orderTab = order of category Tab; tidy = label (unique label for each question in tab); remove questions not relevant to data extraction stage.


write.csv(orderQuestions, "data/OctopusPreclinicalOrderQuestions.csv", row.names = FALSE)

#write csv, then within excel add 
#order = order of question; orderTab = order of category Tab; tidy = label (unique label for each question in tab); remove questions not relevant to data extraction stage.


orderedQuestions<-read.csv("data/OctopusPreclinicalOrderedQuestions.csv")%>%
  select(-Question)

#----3. create annotation summaries
#create summary of all studies, include/exclude status and reason for exclusion if excluded.
invivoAnnotations<-invivoAnnotations%>%
  group_by(StudyIdStr)%>%
  arrange(DateTimeCreated)

invivoAnnotations<-merge(orderedQuestions,invivoAnnotations, by = "QuestionIdStr")

invivoAnnotationsSummary<-invivoAnnotations%>%
  select(StudyIdStr, tidy, Answer, Notes)%>%
  mutate(include_exclude = ifelse(!StudyIdStr %in% unique(invivoAnnotations[invivoAnnotations$QuestionIdStr == invivoExclusionIdStr & !is.na(invivoAnnotations$Answer), "StudyIdStr"]), "include", "exclude"))

includedAnnotationsSummary <- invivoAnnotationsSummary%>%
  filter(include_exclude == "include")%>%
  mutate(exclude_reason = NA)%>%
  select(-tidy, -Answer, -Notes)%>%
  unique()

excludedAnnotationsSummary <- invivoAnnotationsSummary %>%
  filter(include_exclude  == "exclude" & tidy == "excluded_reason")%>%
  mutate ( exclude_reason = ifelse(Answer == "Other", paste0("Other: ", Notes), Answer)) %>%
  select(-tidy, -Answer, -Notes)%>%
  unique()


overall_summary <- rbind(includedAnnotationsSummary,  excludedAnnotationsSummary)

#create to reconcile summary
# toReconcileSummary<-invivoAnnotations%>%
#   filter(!StudyIdStr %in% reconciledAnnotationsSummary$StudyIdStr)%>%
#   group_by(StudyIdStr)%>%
#   arrange(DateTimeCreated)%>%
#   filter(Annotator == last(Annotator))
# 
# toReconcileSummary<-merge(orderedQuestions, toReconcileSummary, by = "QuestionIdStr")%>%
#   mutate(include_exclude = ifelse(QuestionIdStr == invivoExclusionIdStr & !is.na(Answer), "exclude", "include"))%>%
#   select(StudyIdStr, tidy, Answer, Notes, include_exclude) %>% 
#   mutate (exclude_reason = ifelse(include_exclude == "include", NA, Answer))%>%
#   select(-tidy, -Answer, -Notes)%>%
#   unique()
# 
# 
# toReconcileStudyIdStr <- toReconcileSummary%>%
#   select(StudyIdStr)%>%
#   unique()

#create overall summary of all annotated studies in data extraction stage 
# overall_summary<-rbind(toReconcileSummary%>%mutate(reconciliation = "todo"), reconciledAnnotationsSummary%>%mutate(reconciliation = "reconciled"))

overall_summary$exclude_reason <- unlist(overall_summary$exclude_reason)

overall_summary<-data.frame(overall_summary, stringsAsFactors = FALSE)
write.csv(overall_summary, paste0("output/overall_summary", Sys.Date(), ".csv"), row.names = FALSE)


# # functions to summarise annotations by drug, model; and overall quality
# 
# 
# summarise_by_drug<-function(x, drug){
#   summaryByDrug<-x%>%
#   filter(tidy == "intervention_name")%>%
#     group_by(Answer)%>%
#     summarise(nStudies = n_distinct(StudyIdStr), Studies = str_c(StudyIdStr, collapse =" ; "))%>%
#     ungroup()
#   
#   colnames(summaryByDrug)[colnames(summaryByDrug) == "Answer"] <- "Drug"
#   
#   summaryByDrug$Drug<-unlist(summaryByDrug$Drug)
#   
#   summaryByDrug<-summaryByDrug%>%arrange(Drug)
#   
#   return(summaryByDrug)
# }
# 
# summarise_models<-function(x){
#   animalSummary<-x%>%
#     filter(tidy =="model_type")%>%
#     group_by(Answer)%>%
#     summarise(nStudies = n_distinct(StudyIdStr), Studies = str_c(unique(StudyIdStr), collapse =" ; "))%>%
#     ungroup()
#   
#   colnames(animalSummary)[colnames(animalSummary) == "Answer"] <- "Model"
#   
#   animalSummary$Model<-unlist(animalSummary$Model)
#   animalSummary<-animalSummary%>%arrange(Model)%>%mutate(category = "animal")
#   
#   inductionSummary<-x%>%
#     filter(tidy =="induction_mode")%>%
#     group_by(Answer)%>%
#     summarise(nStudies = n_distinct(StudyIdStr), Studies = str_c(unique(StudyIdStr), collapse =" ; "))%>%
#     ungroup()
#   
#   colnames(inductionSummary)[colnames(inductionSummary) == "Answer"] <- "Model"
#   
#   inductionSummary$Model<-unlist(inductionSummary$Model)
#   inductionSummary<-inductionSummary%>%arrange(Model)%>%mutate(category = "inductionMode")
#     
#     
#   mutationSummary<-x%>%
#     filter(tidy =="protein_gene")%>%
#     group_by(Answer)%>%
#     summarise(nStudies = n_distinct(StudyIdStr), Studies = str_c(unique(StudyIdStr), collapse =" ; "))%>%
#     ungroup()
#   
#   colnames(mutationSummary)[colnames(mutationSummary) == "Answer"] <- "Model"
#   
#   mutationSummary$Model<-unlist(mutationSummary$Model)
#   mutationSummary<-mutationSummary%>%arrange(Model)%>%mutate(category = "mutation")
#   
#   sod1Summary<-x%>%
#     filter(tidy =="sod1_copy_number")%>%
#     group_by(Answer)%>%
#     summarise(nStudies = n_distinct(StudyIdStr), Studies = str_c(unique(StudyIdStr), collapse =" ; "))%>%
#     ungroup()
#   colnames(sod1Summary)[colnames(sod1Summary) == "Answer"] <- "Model"
#   
#   sod1Summary$Model<-unlist(sod1Summary$Model)
#   sod1Summary<-sod1Summary%>%arrange(Model)%>%mutate(category = "sod1CopyNumber")
#   
#   
#   modelSummary<-bind_rows(animalSummary, inductionSummary, mutationSummary, sod1Summary)
#   
#   return(modelSummary)
# }
# 
# summarise_quality<-function(x){
#   qualitySummary<-x%>%
#     filter(orderTab == 6)%>%
#     filter(tidy != "exp_label" & tidy != "exp_cohorts")%>%
#     select(StudyIdStr, tidy, Answer)%>%
#     unique()%>%
#     mutate(Score = ifelse(Answer == "No", 0, ifelse(Answer == "Yes", 1, 0.5 ))) %>%
#     select(-Answer)%>%
#     pivot_wider(names_from = tidy, values_from = Score, values_fn = min)
#   qualitySummary<-qualitySummary%>%
#     mutate(totalQuality = rowSums(qualitySummary[,2:10]))
#   
#   return(qualitySummary)
# }

#generate summaries by Drug, by model and quality using summarise functions 
# reconciledStudiesByDrug<-summarise_by_drug(reconciledAnnotations)
# reconciledStudiesByModel<-summarise_models(reconciledAnnotations)
# reconciledStudiesQuality<-summarise_quality(reconciledAnnotations)
# reconciledOverallQuality<-data.frame(meanScore = colMeans(reconciledStudiesQuality[,-1]), sd = colSds(as.matrix(reconciledStudiesQuality[,-1])))%>%round(2)
#                                        

#----4. generate full reconciled annotation list per tab
invivoAnnotations<-invivoAnnotations%>%arrange(orderTab, order)


split <- divideByTab(invivoAnnotations)

model_info <- SummariseModelTab(invivoAnnotations, split)


model_labels <- invivoAnnotations%>%
  filter(orderTab == 2, order == 1)%>%
  select(idStr, Answer)%>%
  rename(ModelIdStr = idStr)%>%
  rename(model_label = Answer)
model_info<-left_join(model_info, model_labels, by = "ModelIdStr")
treat_info <- SummariseTreatTab(invivoAnnotations, split)
outcome_info <- SummariseOutcomeTab(invivoAnnotations, split)
outcome_labels <- invivoAnnotations%>%
  filter(orderTab == 4, order == 1)%>%
  select(idStr, Answer)%>%
  rename(OutcomeIdStr = idStr)%>%
  rename(outcome_label = Answer)
outcome_info <- left_join(outcome_info, outcome_labels, by = "OutcomeIdStr")
cohort_info <- SummariseCohortTab(invivoAnnotations, split)
experiment_info <- SummariseExperimentTab(invivoAnnotations, split)

combined_data <- left_join(experiment_info, cohort_info, by = "CohortIdStr")
combined_data <- left_join(combined_data, model_info, by = "ModelIdStr")
combined_data <- left_join(combined_data, treat_info, by = "TreatmentIdStr")
combined_data <- left_join(combined_data, outcome_info, by = "OutcomeIdStr")

#----5. get full outcome data and combine data
outcome_data <- includedOutcomes%>%
  select(OutcomeIdStr, CohortIdStr, ExperimentIdStr, StudyIdStr, AverageType, Average, Units, ErrorType, Error, Time, NumberOfAnimals)

full_data <- left_join(combined_data, outcome_data, by = c("OutcomeIdStr", "CohortIdStr", "ExperimentIdStr", "StudyIdStr"))%>%arrange(StudyIdStr, ExperimentIdStr)
full_data <- full_data%>%
  unique()

# full_data_csv <- apply(full_data, 2, as.character)


qualityCols <- c("coi", "temp_control", "sample_size", "concealment", "peer_review", "welfare", "appropriate_control", "blinding", "randomisation")



fullPublicationData <- function(x){
  publicationData<-x%>%
    # mutate(across(qualityCols,  ~ qualityScore(.x)))%>%
    mutate(ModelCategory = ifelse(ModelCategory == "ModelControl", "Control", "Disease Model"))%>%
    mutate(TreatCategory = ifelse(TreatCategory == "TreatControl", "Control", "Treatment"))
  
  publicationData1 <- invivoStudies%>%
    select(idStr,
           Title, 
           Abstract, 
           Url,
           Year,
           Author,
           Journal,
           PdfRelativePath)%>%
    rename(StudyIdStr = idStr)%>%
    rename(DOI = Url)
  
  publicationData <- right_join(publicationData1, publicationData, by = "StudyIdStr")%>%
    unique()%>%
    mutate(model = paste0(ModelCategory, 
                          " - ",
                          "animal: ",
                          animal_species,
                          "; strain:",
                          animal_strain, 
                          "; sex:",
                          animal_sex, 
                          "; ages:",
                          animal_age,
                          "; MS model type:",
                          ms_model_type,
                          ifelse(!is.na(eae_model), paste0("; EAE model type:", eae_model), ""),
                          "; time between induction and outcome:",
                          time_from_induction))%>%
    mutate(treatment = paste0(TreatCategory, 
                              " - ",
                              intervention_name,
                              ifelse(!is.na(intervention_name), paste0(" ( dose:", 
                                                                       dose, 
                                                                       "; timing: ", 
                                                                       timing,  
                                                                       "; route: ",
                                                                       route,
                                                                       "; duration:",
                                                                       duration,
                                                                       ")")
                                     , "")
    ))%>%
    mutate(outcome = paste0(outcome_label, 
                            "outcome type:",
                            outcome_type 
    ))
  
  studyData <- publicationData%>%
    select(StudyIdStr, ExperimentIdStr, Author, Year)%>%unique()%>%group_by(StudyIdStr)%>%
    mutate(Experiment = paste0(Author, " ", Year, letters[seq_along(ExperimentIdStr)]))%>%
    select(StudyIdStr, ExperimentIdStr, Experiment)
  
  
  publicationData <- right_join(studyData, publicationData, by = c("ExperimentIdStr", "StudyIdStr"))
  
  return(publicationData)
}


invivoPublicationData <- fullPublicationData(full_data)

invivoPublicationDataCSV<- apply(invivoPublicationData, 2, as.character)

write.csv(invivoPublicationDataCSV, paste0('output/', Sys.Date(), 'octopusPreclinicalPublicationData.csv'), row.names = FALSE)


#-----6. Calculate effect sizes

data <- invivoPublicationData%>%
  select(StudyIdStr,
         ExperimentIdStr,
         Experiment,
         CohortIdStr,
         ModelIdStr,
         TreatmentIdStr,
         cohort_numbers,
         OutcomeIdStr,
         model_label,
         ms_model_type,
         ModelCategory,
         intervention_name, 
         dose, 
         timing, 
         TreatCategory,
         outcome_label,
         outcome_type,
         greater_worse,
         Time,
         Average,
         AverageType,
         Units,
         Error,
         ErrorType,
         NumberOfAnimals,
         model,
         treatment, 
         outcome
  )

#1. Calculate effect sizes for continuous outcomes----
## 1.1 For data presented as mean----

meanOutcomeData <- data %>%
  filter(!is.na(Average))%>%
  filter(AverageType == "Mean")%>%
  mutate(ExperimentOutcomeIdStr = paste(ExperimentIdStr, OutcomeIdStr, sep = "-"))

for (i in unique(meanOutcomeData$ExperimentOutcomeIdStr)){
  expData<- meanOutcomeData%>%
    filter(ExperimentOutcomeIdStr == i)
  
  if(!is.null(expData)){
    expTimePoint<-expData%>%
      group_by(StudyIdStr, ExperimentIdStr, ModelIdStr, TreatmentIdStr, OutcomeIdStr)%>%
      summarise(NTimePoint = length(unique(Time)))
    
    expData <- left_join(expData, expTimePoint, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "TreatmentIdStr", "OutcomeIdStr"))
    
    if (max(expData$NTimePoint==1)){
      # 1.1.1 analysis for data presented as single time points-----  
      # 1.1.1.1 generate control and treatment group data frames----
      expControlDF<- expData%>%
        filter(ModelCategory == "Disease Model")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
        summarise(AverageControl = Average[TreatCategory == "Control"],
                  NControl = NumberOfAnimals[TreatCategory == "Control"],
                  NRxGroups = length(unique(CohortIdStr[TreatCategory == "Treatment"])),
                  errorControl = Error[TreatCategory == "Control"],
                  errorTypeControl = ErrorType[TreatCategory == "Control"])%>%
        mutate(NTrueControl = NControl/NRxGroups)%>%
        mutate(sdControl = ifelse(errorTypeControl == "SD", errorControl, ifelse(errorTypeControl == "SEM", errorControl*sqrt(NControl), NA)))
      
      expRxDF<- expData%>%
        filter(ModelCategory == "Disease Model", TreatCategory == "Treatment")%>%  
        filter(!intervention_name %in% "Other")%>%
        group_by(StudyIdStr, ExperimentIdStr, ModelIdStr, OutcomeIdStr, TreatmentIdStr)%>%
        summarise(AverageRx = Average,
                  NRx = NumberOfAnimals,
                  errorRx = Error,
                  errorTypeRx = ErrorType,
                  greater_worse = greater_worse)%>%
        mutate(sdRx = ifelse(errorTypeRx == "SD", errorRx, ifelse(errorTypeRx == "SEM", errorRx*sqrt(NRx), NA)))
      
      # 1.1.1.2 calculate single timepoint SMD ES----
      
      smdDataFrame <- inner_join(expControlDF, expRxDF, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
      
      smdDataFrame <-smdDataFrame %>%   
        mutate(ESdir = ifelse(greater_worse == "TRUE", 
                              ifelse(AverageRx>AverageControl, -1, 1), 
                              ifelse(AverageRx>AverageControl, 1, -1)), 
               Ntotal = NTrueControl + NRx)%>%
        mutate(SDpooled = sqrt( (((NTrueControl -1)*sdControl^2) + (NRx -1)*sdRx^2)/(Ntotal - 2)  ) )%>%
        mutate(ES = abs(((AverageControl - AverageRx) / SDpooled) * (1 - (3/(4*Ntotal -9))))*ESdir)%>%
        mutate(seES = sqrt( (Ntotal/(NRx * NTrueControl)) + (ES^2/(2*(Ntotal -3.94)))),
               analysis = "SMD")%>%
        unique()
      
      if(!is.null(smdDataFrame))  if(!exists('smdSingleTpDF')) smdSingleTpDF <- smdDataFrame else smdSingleTpDF <- rbind(smdSingleTpDF, smdDataFrame)
      
      # 1.1.1.3 generate sham data frame----
      expShamDF<-expData %>%
        filter(ModelCategory == "Control" & TreatCategory == "Control")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
        summarise(AverageSham = Average,
                  errorSham = Error, 
                  errorTypeSham = ErrorType)
      
      expShamDF <- subset(expShamDF, select = -ModelIdStr )
      
      # 1.1.1.4 if sham present then calculate NMD ES----
      if(nrow(expShamDF)>0){
        
        nmdControlDF<- left_join(expControlDF, expShamDF, by = c("StudyIdStr", "ExperimentIdStr", "OutcomeIdStr"))
        
        nmdDataFrame <- inner_join(nmdControlDF, expRxDF, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
        
        nmdDataFrame <- nmdDataFrame%>%
          mutate(ESdir = ifelse(greater_worse == "TRUE", 
                                ifelse(AverageRx>AverageControl, -1, 1), 
                                ifelse(AverageRx>AverageControl, 1, -1)))%>%
          mutate(
            ES = abs((((AverageControl-AverageSham)-(AverageRx - AverageSham))/(AverageControl - AverageSham)))*ESdir,
            nsdControl =  abs(sdControl/(AverageControl - AverageSham)),
            nsdRx = abs(sdRx/(AverageControl - AverageSham)))%>%
          mutate(seES = sqrt((nsdControl^2/NTrueControl) + (nsdRx^2 / NRx) ),
                 analysis = "NMD")
        
        
        
        if(!is.null(nmdDataFrame))  if(!exists('nmdSingleTpDF')) nmdSingleTpDF <- nmdDataFrame else nmdSingleTpDF <- rbind(nmdSingleTpDF, nmdDataFrame)
      } 
      
      
    }else{
      
      # 1.1.2 analysis for data presented with multiple time points----
      # 1.1.2.1 Calculate AUC and SD for AUC for each intervention group ----
      # Control group
      expControlDF1<- expData%>%
        filter(ModelCategory == "Disease Model")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr, NTimePoint)%>%
        summarise(NControl = NumberOfAnimals[TreatCategory == "Control"],
                  NRxGroups = length(unique(CohortIdStr[TreatCategory == "Treatment"]))
        )%>%
        mutate(NTrueControl = NControl/NRxGroups)%>%
        unique()
      
      expControlDF2<-expData%>%
        filter(TreatCategory == "Control" & ModelCategory == "Disease Model")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr, NumberOfAnimals)%>%
        summarise(overallMeanControl = mean(Average),
                  FTPControl = min(Time),
                  LTPControl = max(Time),
                  meanFTPControl = Average[Time == FTPControl],
                  meanLTPControl = Average[Time == LTPControl],
                  errorLTPControl = Error[Time == LTPControl],
                  errorTypeLTPControl = ErrorType[Time==LTPControl])%>%
        mutate(sdLTPControl = ifelse(errorTypeLTPControl == "SD", errorLTPControl, ifelse(errorTypeLTPControl == "SEM", errorLTPControl*sqrt(NumberOfAnimals), NA))
        )
      
      expControlDF3 <- expData%>%
        filter(TreatCategory == "Control"& ModelCategory == "Disease Model")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
        mutate(meanDifferenceSqControl = (Average - unique(expControlDF2$overallMeanControl))^2,
               sdControl = ifelse(ErrorType == "SD", Error, ifelse(ErrorType == "SEM", Error*sqrt(NumberOfAnimals), NA)))%>%
        summarise(sumMeanDifferenceSqControl = sum(meanDifferenceSqControl),
                  sumVarianceControl = sum((sdControl^2)))
      
      expControlDF<-merge(expControlDF1, expControlDF2, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
      
      expControlDF<-merge(expControlDF, expControlDF3, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
      
      expControlDF<-expControlDF%>%
        mutate(aucControl = (NTimePoint*overallMeanControl) - (0.5*(meanFTPControl + meanLTPControl)),
               sdAucControl = sqrt(sumMeanDifferenceSqControl + sumVarianceControl))%>%
        select(-NumberOfAnimals)%>%
        select(-NTimePoint)
      
      #Rx group
      expRxDF1<-expData %>%
        filter(!intervention_name %in% "other (free text)")%>%
        filter(ModelCategory == "Disease Model", TreatCategory == "Treatment")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr, TreatmentIdStr, NumberOfAnimals, NTimePoint)%>%
        
        
       
         summarise(NRx = NumberOfAnimals,
                  overallMeanRx = mean(Average),
                  FTPRx = min(Time),
                  LTPRx = max(Time)
                  # ,
                  # meanFTPRx = Average[Time == FTPRx],
                  # meanLTPRx = Average[Time == LTPRx],
                  # errorLTPRx = Error[Time == LTPRx],
                  # errorTypeLTPRx = ErrorType[Time==LTPRx]
                  )
      
      # %>%
        unique()%>%
        mutate(sdLTPRx = ifelse(errorTypeLTPRx == "SD", errorLTPRx, ifelse(errorTypeLTPRx == "SEM", errorLTPRx*sqrt(NumberOfAnimals), NA))
        )%>%
        select(-NTimePoint)
      
      expRxDF2 <- expData%>%
        filter(ModelCategory == "Disease Model", TreatCategory == "Treatment")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
        mutate(meanDifferenceSqRx = (Average - unique(expRxDF1$overallMeanRx))^2,
               sdRx = ifelse(ErrorType == "SD", Error, ifelse(ErrorType == "SEM", Error*sqrt(NumberOfAnimals), NA)))%>%
        summarise(sumMeanDifferenceSqRx = sum(meanDifferenceSqRx),
                  sumVarianceRx = sum((sdRx^2)))
      
      expRxDF<-merge(expRxDF1, expRxDF2, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))%>%
        select(-NumberOfAnimals)
      
      expRxDF<-expRxDF%>%
        mutate(aucRx = (NTimePoint*overallMeanRx) - (0.5*(meanFTPRx + meanLTPRx)),
               sdAucRx = sqrt(sumMeanDifferenceSqRx + sumVarianceRx))
      
      expDataFrame <- inner_join(expControlDF, expRxDF, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
      
      # 1.1.2.2 add direction of effect----
      directionDataFrame<-data%>%
        ungroup()%>%
        select(OutcomeIdStr, greater_worse)
      
      # 1.1.2.3 calculate smd effect sizes for auc----
      smdDataFrameAUC <- left_join(expDataFrame, directionDataFrame, by ="OutcomeIdStr")%>%unique()
      
      smdDataFrameAUC <-smdDataFrameAUC %>%   
        mutate(ESdir = ifelse(greater_worse == "TRUE", 
                              ifelse(aucRx>aucControl, -1, 1), 
                              ifelse(aucRx>aucControl, 1, -1)), 
               Ntotal = NTrueControl + NRx)%>%
        mutate(SDpooled = sqrt( (((NTrueControl -1)*sdAucControl^2) + (NRx -1)*sdAucRx^2)/(Ntotal - 2)  ) )%>%
        mutate(ES = abs(((aucControl - aucRx) / SDpooled) * (1 - (3/(4*Ntotal -9))))*ESdir)%>%
        mutate(seES = sqrt( (Ntotal/(NRx * NTrueControl)) + (ES^2/(2*(Ntotal -3.94)))),
               analysis = "aucSMD")%>%
        unique()
      
      if(!is.null(smdDataFrameAUC))  if(!exists('smdMultipleTpDF')) smdMultipleTpDF <- smdDataFrameAUC else smdMultipleTpDF <- rbind(smdMultipleTpDF, smdDataFrameAUC)
      
      # 1.1.2.4 generate sham group data frame----
      
      expShamDF1<-expData %>%
        filter(ModelCategory == "Control" & TreatCategory == "Control")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr, NumberOfAnimals, NTimePoint)%>%
        summarise(NSham = NumberOfAnimals,
                  overallMeanSham = mean(Average),
                  FTPSham = min(Time),
                  LTPSham = max(Time),
                  meanFTPSham = Average[Time == FTPSham],
                  meanLTPSham = Average[Time == LTPSham],
                  errorLTPSham = Error[Time == LTPSham],
                  errorTypeLTPSham = ErrorType[Time==LTPSham])%>%
        mutate(sdLTPSham = ifelse(errorTypeLTPSham == "SD", errorLTPSham, ifelse(errorTypeLTPSham == "SEM", errorLTPSham*sqrt(NumberOfAnimals), NA))
        )%>%unique()%>%
        select(-NTimePoint)
      
      expShamDF2 <- expData%>%
        filter(TreatCategory == "Control"& ModelCategory == "Control")%>%
        group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
        mutate(meanDifferenceSqSham = (Average - unique(expShamDF1$overallMeanSham))^2,
               sdSham = ifelse(ErrorType == "SD", Error, ifelse(ErrorType == "SEM", Error*sqrt(NumberOfAnimals), NA)))%>%
        summarise(sumMeanDifferenceSqSham = sum(meanDifferenceSqSham),
                  sumVarianceSham = sum((sdSham^2)))%>%unique()
      expShamDF<-merge(expShamDF1, expShamDF2, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
      
      expShamDF<-expShamDF%>%
        mutate(aucSham = (NTimePoint*overallMeanSham) - (0.5*(meanFTPSham + meanLTPSham)),
               sdAucSham = sqrt(sumMeanDifferenceSqSham + sumVarianceSham))%>%
        select(-NumberOfAnimals)
      
      expShamDF <- subset(expShamDF, select = -ModelIdStr )%>%
        select(-NTimePoint)
      
      
      # 1.1.2.5 if sham data present, calculate NMD effect size for AUC ----
      
      if (nrow(expShamDF)>0) {
        
        nmdControlDFAUC<- left_join(expControlDF, expShamDF, by = c("StudyIdStr", "ExperimentIdStr", "OutcomeIdStr"))
        
        nmdDataFrameAUC <- inner_join(nmdControlDFAUC, expRxDF, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
        
        
        nmdDataFrameAUC <- left_join(nmdDataFrameAUC, directionDataFrame, by ="OutcomeIdStr")%>%unique()
        
        nmdDataFrameAUC <- nmdDataFrameAUC%>%
          mutate(ESdir = ifelse(greater_worse == "TRUE", 
                                ifelse(aucRx>aucControl, -1, 1), 
                                ifelse(aucRx>aucControl, 1, -1)))%>%
          mutate(
            ES = abs((((aucControl-aucSham)-(aucRx - aucSham))/(aucControl - aucSham)))*ESdir,
            
            nsdAucControl =  abs(sdAucControl/(aucControl - aucSham)),
            nsdAucRx = abs(sdAucRx/(aucControl - aucSham))
          )%>%
          mutate(seES = sqrt((nsdAucControl^2/NTrueControl) + (nsdAucRx^2 / NRx) ),
                 analysis = "aucNMD")
        
        nmdDataFrameAUC <- nmdDataFrameAUC%>%unique()
        
        if(!is.null(nmdDataFrameAUC))  if(!exists('nmdMultipleTpDF')) nmdMultipleTpDF <- nmdDataFrameAUC else nmdMultipleTpDF <- rbind(nmdMultipleTpDF, nmdDataFrameAUC)
      } 
    } 
  }
}

## 1.2 For data presented as median ----

medianOutcomeData <- data %>%
  filter(!is.na(Average))%>%
  filter(AverageType == "Median")%>%
  filter(ModelCategory == "Disease Model")%>%
  mutate(ExperimentOutcomeIdStr = paste(ExperimentIdStr, OutcomeIdStr, sep = "-"))


for (i in medianOutcomeData$ExperimentOutcomeIdStr){
  expData<- medianOutcomeData%>%
    filter(ExperimentOutcomeIdStr == i)
  expControlDF<- expData%>%
    group_by(StudyIdStr,ExperimentIdStr, ModelIdStr, OutcomeIdStr)%>%
    summarise(AverageControl = Average[TreatCategory == "Control"],
              NControl = NumberOfAnimals[TreatCategory == "Control"],
              NRxGroups = length(unique(CohortIdStr[TreatCategory == "Treatment"])))%>%
    mutate(NTrueControl = NControl/NRxGroups)
  
  expRxDF<- expData%>%
    filter(!intervention_name %in% "other (free text)")%>%
    group_by(StudyIdStr, ExperimentIdStr, ModelIdStr, TreatmentIdStr, OutcomeIdStr,greater_worse)%>%
    summarise(AverageRx = Average[TreatCategory == "Treatment"],
              NRx = NumberOfAnimals[TreatCategory == "Treatment"])
  
  expDataFrame <- inner_join(expControlDF, expRxDF, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "OutcomeIdStr"))
  
  expDataFrame <- expDataFrame%>%
    mutate(ESdir = ifelse(greater_worse == "TRUE", 
                          ifelse(AverageRx>AverageControl, -1, 1), 
                          ifelse(AverageRx>AverageControl, 1, -1)))
  
  
  expMedianDF <- expDataFrame%>%
    group_by(StudyIdStr, ExperimentIdStr, ModelIdStr, TreatmentIdStr, OutcomeIdStr)%>%
    summarise(ES = abs(log(AverageRx/AverageControl))*ESdir,
              seES = 1/sqrt(NTrueControl + NRx))
  if(!is.null(expMedianDF))  if(!exists('medianOutcomeDF')) medianOutcomeDF <- expMedianDF else medianOutcomeDF <- rbind(medianOutcomeDF, expMedianDF)
  
  medianOutcomeDF <-medianOutcomeDF%>%unique()%>%mutate(analysis = "median")
}

addStudyCharacteristics<- function(df){
  df <- df%>%
    select(StudyIdStr,
           ExperimentIdStr,
           ModelIdStr,
           TreatmentIdStr,
           OutcomeIdStr,
           ES,
           seES,
           analysis)%>%
    unique()
  
  expCharacteristics <- data %>%
    select(StudyIdStr,
           ExperimentIdStr,
           ModelIdStr,
           TreatmentIdStr,
           OutcomeIdStr,
           Experiment,
           model_label,
           model,
           intervention_name,
           dose,
           timing, 
           outcome_label,
           outcome_type)
  
  outputDF <- right_join(expCharacteristics, df, by = c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "TreatmentIdStr", "OutcomeIdStr"))%>%
    #  select(- c("StudyIdStr", "ExperimentIdStr", "ModelIdStr", "TreatmentIdStr", "OutcomeIdStr"))%>%
    unique()
  
  rownames(outputDF) <- c()
  return(outputDF)
}

#2. Generate output data frame----

if(!exists("nmdSingleTpDF")) nmdSingleTpDF <- NULL
if(!exists("nmdMultipleTpDF")) nmdMultipleTpDF <- NULL
if(!exists("smdSingleTpDF")) smdSingleTpDF <- NULL
if(!exists("smdMultipleTpDF")) smdMultipleTpDF <- NULL
if(!exists("medianOutcomeDF")) medianOutcomeDF <- NULL

outcomeDFList <- list(nmdSingleTpDF,  smdSingleTpDF, smdMultipleTpDF, medianOutcomeDF,nmdMultipleTpDF)

for (i in outcomeDFList){
  if(!is.null(i)){
    df <- addStudyCharacteristics(i)
    if(!exists('outputDF')) outputDF <- df else outputDF <- bind_rows(outputDF, df)
  } else next
}

invivoSummary <- left_join(outputDF, unique(invivoPublicationData[, c("ExperimentIdStr", qualityCols, "Title", "Author", "Year", "DOI", "PdfRelativePath")]), by = "ExperimentIdStr")
invivoSummary<- left_join(invivoSummary, unique(invivoPublicationData[, c("ModelIdStr", "ModelCategory", "ms_model_type")]), by = "ModelIdStr")
invivoSummary <- invivoSummary%>%
  filter(is.numeric(c(ES, seES)))%>%
  filter(ES != "Inf")%>%
  filter(seES != "Inf")

calculateQuality <- function(invivoSummary){
  qualityDF <- invivoSummary[ ,c("ExperimentIdStr", qualityCols)]
  qualityDF[, qualityCols] <- lapply(qualityDF[, qualityCols], function(x) ifelse(x == "No", 0, ifelse(x == "Yes", 1, 0.5) )) 
  qualityDF$totalQuality  <- rowSums(qualityDF[ ,qualityCols])
  invivoSummary <- left_join(invivoSummary, qualityDF[, c("ExperimentIdStr", "totalQuality")], by = "ExperimentIdStr")%>%
    unique()
  
  return(invivoSummary)
}

invivoSummary <- calculateQuality(invivoSummary)

# invivoSummary <- invivoSummary %>% mutate(across(!where(is.numeric), as.character))



invivoSummary$intervention_name <- tolower(invivoSummary$intervention_name)
invivoDrugList <- invivoSummary %>% ungroup()%>% select(intervention_name)%>%unique()%>% arrange(intervention_name)

