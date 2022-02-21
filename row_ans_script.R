


# Rendering script for row_ans() function for several participants:

rowNum <- c(1, 2, 3)

sapply(rowNum, function(x) {
  rmarkdown::render(input = "Responses_Participant.Rmd", 
                    output_file = sprintf("Responses_Participant_row_%s.html", x),
                    params = list(rowNum = x))
})
