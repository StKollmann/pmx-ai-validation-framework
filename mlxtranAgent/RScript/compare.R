args = commandArgs(trailingOnly=TRUE)

set.seed(123)

suppressMessages(suppressWarnings(library(lixoftConnectors)))
suppressMessages(suppressWarnings(initializeLixoftConnectors("simulx")))

newProject(modelFile = args[1])

defineTreatmentElement(name = "dose", element = list(data = data.frame(time = 0, amount = 100)))
setGroupElement("simulationGroup1", elements = "dose")

params <- getIndividualElements()$Parameters$data
names <- names(params)
params <- runif(length(params))
names(params) <- names
defineIndividualElement(name = "random", element = data.frame(as.list(params)))
setGroupElement("simulationGroup1", elements = "random")

output_variables <- sapply(getOutputElements(), "[[", "output")
for (output in output_variables) {
    defineOutputElement(name = output, element = list(data = data.frame(time = 12), output = output))
}
setGroupElement("simulationGroup1", elements = unname(output_variables))

runSimulation()
res1 <- getSimulationResults()$res


newProject(modelFile = args[2])

defineTreatmentElement(name = "dose", element = list(data = data.frame(time = 0, amount = 100)))
setGroupElement("simulationGroup1", elements = "dose")

defineIndividualElement(name = "random", element = data.frame(as.list(params)))
setGroupElement("simulationGroup1", elements = "random")

for (output in output_variables) {
    defineOutputElement(name = output, element = list(data = data.frame(time = 12), output = output))
}
setGroupElement("simulationGroup1", elements = unname(output_variables))

runSimulation()
res2 <- getSimulationResults()$res

results <- sapply(names(res1), function(name) {
  all.equal(res1[[name]], res2[[name]])
}, simplify = FALSE)

# Print per-output comparison details
for (name in names(results)) {
  val <- results[[name]]
  if (isTRUE(val)) {
    cat(sprintf("  %s: MATCH\n", name))
  } else {
    cat(sprintf("  %s: %s\n", name, paste(val, collapse="; ")))
  }
}

pass <- all(sapply(results, is.logical))
cat(sprintf("PASS=%s\n", toupper(as.character(pass))))
print(pass)