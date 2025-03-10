testthat::test_that("multiplication works", {

  testthat::expect_no_error({
    utils::capture.output(
      type = "message",
      {
        # ------------ Set up a bare minimal example pipeline ---------------
        root_path <- tempdir()
        pipeline_root_folder <- file.path(root_path, "modules")

        # create pipeline folder
        pipeline_path <- pipeline_create_template(
          root_path = pipeline_root_folder, pipeline_name = "raveio_demo",
          overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")

        on.exit({
          unlink(pipeline_path, recursive = TRUE)
        }, add = TRUE)

        # Set initial user inputs
        yaml::write_yaml(
          x = list(
            n = 100,
            pch = 16,
            col = "steelblue"
          ),
          file = file.path(pipeline_path, "settings.yaml")
        )

        # build the pipeline for the first time
        # this is a one-time setup
        pipeline_build(pipeline_path)

        # Temporarily redirect the pipeline project root
        # to `root_path`
        old_opt <- options("raveio.pipeline.project_root" = root_path)
        on.exit({ options(old_opt) }, add = TRUE)

        # Compile the pipeline document
        # rmarkdown::render(
        #   input = file.path(pipeline_path, "main.Rmd"),
        #   output_dir = pipeline_path,
        #   knit_root_dir = pipeline_path,
        #   intermediates_dir = pipeline_path, quiet = TRUE
        # )
        pipeline_render(
          module_id = "raveio_demo",
          project_path = root_path
        )

        # Reset options
        # no need to on.exit because it's called (see near Line 37)
        options("raveio.pipeline.project_root" = NULL)

        # --------------------- Example starts ------------------------

        # Load pipeline
        pipeline <- pipeline(
          pipeline_name = "raveio_demo",
          paths = pipeline_root_folder,
          temporary = TRUE
        )

        # Check which pipeline targets to run
        pipeline$target_table

        # Run to `plot_data`, RAVE pipeline will automatically
        # calculate which up-stream targets need to be updated
        # and evaluate these targets
        pipeline$run("plot_data")

        # Customize settings
        pipeline$set_settings(pch = 2)

        # Run again with the new inputs, since input_data does not change,
        # the pipeline will skip that target automatically
        pipeline$run("plot_data")

        # Read intermediate data
        head(pipeline$read("input_data"))

        # or use `[]` to get results
        pipeline[c("n", "pch", "col")]
        pipeline[-c("input_data")]

        # Check evaluating status
        pipeline$progress("details")

        # result summary & cache table
        pipeline$result_table

        # visualize the target dependency graph
        # pipeline$visualize(glimpse = TRUE)

        # --------------------- Clean up ------------------------
        unlink(pipeline_path, recursive = TRUE)
      }
    )
  })

})
