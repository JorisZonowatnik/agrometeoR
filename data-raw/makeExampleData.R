# # Loading the agrometeoR library
# devtools::load_all()
#
# ex_dfrom = "2017-03-04T15:00:00Z"
# devtools::use_data(ex_dfrom, overwrite = TRUE)
#
# ex_dto = "2017-03-04T15:00:00Z"
# devtools::use_data(ex_dto, overwrite = TRUE)
#
# # examplative expected raw API output
# ex_getData = agrometeoR::getData(dfrom = ex_dfrom, dto = ex_dto)
# devtools::use_data(ex_getData, overwrite = TRUE)
#
# # examplative expected typed API output
# ex_typeData = agrometeoR::typeData(ex_dataAPIraw)
# devtools::use_data(ex_typeData, overwrite = TRUE)
#
# # examplative makeDataset output
# ex_makeDataset = agrometeoR::makeDataset(dfrom = ex_dfrom, dto = ex_dto)
# devtools::use_data(ex_makeDataset, overwrite = TRUE)
#
# # examplative task
# ex_maskeTasks = agrometeoR::makeTasks(dataset = ex_makeDataset, target = "tsa")
# devtools::use_data(ex_makeTasks, overwrite = TRUE)
#
# # examplative learner
# ex_makeLearner = mlr::makeLearner("regr.lm", id = "multipleLinearReg", predict.type = "se" )
# devtools::use_data(ex_makeLearner, overwrite = TRUE)
#
# # examplative trained learner (model)
# ex_makeModel = makeModel(task = ex_maskeTasks[[1]], learner = ex_makeLearner)
# devtools::use_data(ex_makeModel, overwrite = TRUE)
#
# # examplative spatialization
# ex_makeSpatialization = makeSpatialization(model = ex_makeModel$model)
# devtools::use_data(ex_makeSpatialization, overwrite = TRUE)
#
# # examplative exported spatialization to CSV
# ex_exportSpatialization = exportSpatialization(spatialized = ex_makeSpatialization)
# devtools::use_data(ex_exportSpatialization, overwrite = TRUE)