# _targets.R file
library(targets)
for (x in list.files(here::here("R"), pattern = "R$|r$", full.names = TRUE)) {
  source(x)
}
targets::tar_option_set(
  # attach packages in targets
  packages = c("ggplot2", "tibble", "projr")
)
list(

  # specify _projr directories
  # ------------------
  tar_target(
    dir_data_raw, projr_dir_get("data-raw"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_cache, projr_dir_get("cache"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_output, projr_dir_get("output"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    dir_docs, projr_dir_get("docs"),
    cue = tar_cue(mode = "always")
  ),

  # do something
  # ------------------
  tar_target(
    path_p_intro_cache,
    {
      sigma_mat <- matrix(c(3, 2, 2, 2), byrow = TRUE, ncol = 2)
      set.seed(1)
      mvn_mat <- MASS::mvrnorm(
        n = 20, mu = rep(0, 2), Sigma = sigma_mat
      )
      unif_mat <- matrix(
        c(runif(5, 1.5, 3), runif(5, 0.5, 1)),
        ncol = 2, byrow = FALSE
      )
      orig_mat <- mvn_mat |> rbind(unif_mat)
      orig_mat[, 2] <- pmin(orig_mat[, 2], 1.75)
      orig_mat[, 2] <- pmax(orig_mat[, 2], -1.9)
      orig_mat[, 1] <- orig_mat[, 1] - mean(orig_mat[, 1])
      orig_mat[, 2] <- orig_mat[, 2] - mean(orig_mat[, 2])
      proj_vec <- c(1, 0)
      orig_mat <- cbind(orig_mat[, 2], orig_mat[, 1])

      eigen_vector <- eigen(t(orig_mat) %*% orig_mat)$vectors[, 1]
      plot_tbl_full <- tibble::as_tibble(orig_mat) |>
        dplyr::rename(x = V1, y = V2) |>
        dplyr::mutate(
          type = "orig"
        ) |>
        dplyr::bind_rows(
          tibble::as_tibble(calc_proj(orig_mat, c(1, 0))) |>
            dplyr::rename(x = V1, y = V2) |>
            dplyr::mutate(
              type = "proj_x"
            )
        ) |>
        dplyr::bind_rows(
          tibble::as_tibble(calc_proj(orig_mat, c(0, 1))) |>
            dplyr::rename(x = V1, y = V2) |>
            dplyr::mutate(
              type = "proj_y"
            )
          # projection onto eigenvector
        ) |>
        dplyr::bind_rows(
          tibble::as_tibble(calc_proj(orig_mat, eigen_vector)) |>
            dplyr::rename(x = V1, y = V2) |>
            dplyr::mutate(
              type = "proj_eigen"
            )
        )
      plot_tbl_orig <- plot_tbl_full |>
        dplyr::filter(type == "orig")
      plot_tbl_x <- plot_tbl_orig |>
        dplyr::select(-type) |>
        dplyr::bind_cols(
          plot_tbl_full |>
            dplyr::filter(type == "proj_x") |>
            dplyr::select(-type) |>
            dplyr::rename(proj_x = x, proj_y = y)
        )
      plot_tbl_y <- plot_tbl_orig |>
        dplyr::select(-type) |>
        dplyr::bind_cols(
          plot_tbl_full |>
            dplyr::filter(type == "proj_y") |>
            dplyr::select(-type) |>
            dplyr::rename(proj_x = x, proj_y = y)
        )
      plot_tbl_eigen <- plot_tbl_orig |>
        dplyr::select(-type) |>
        dplyr::bind_cols(
          plot_tbl_full |>
            dplyr::filter(type == "proj_eigen") |>
            dplyr::select(-type) |>
            dplyr::rename(proj_x = x, proj_y = y)
        )
      path_plot_orig <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-orig.png"
      )
      lim_vec <- range(c(plot_tbl_orig$x, plot_tbl_orig$y))
      p_orig <- ggplot() +
        cowplot::theme_cowplot(font_size = 29) +
        cowplot::background_grid(major = "xy") +
        geom_vline(xintercept = 0, size = 1, col = "gray65") +
        geom_hline(yintercept = 0, size = 1, col = "gray65") +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
        ) +
        geom_point(
          data = plot_tbl_orig, aes(x = x, y = y), color = "black"
        ) +
        coord_equal() +
        lims(x = lim_vec, y = lim_vec)
      cowplot::ggsave2(
        path_plot_orig, p_orig,
        width = 5, height = 5, dpi = 300
      )
      # projections
      # --------------------

      # projection onto x
      p_x <- p_orig +
        geom_hline(yintercept = 0, size = 1) +
        geom_segment(
          data = plot_tbl_x, aes(x = x, y = y, xend = proj_x, yend = proj_y),
          col = "dodgerblue",
          arrow = arrow(length = unit(0.03, "npc"))
        )
      path_plot_x <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-x.png"
      )
      cowplot::ggsave2(
        path_plot_x, p_x,
        width = 5, height = 5, dpi = 300
      )
      # projection onto y
      p_y <- p_orig +
        geom_hline(yintercept = 0, size = 1) +
        geom_segment(
          data = plot_tbl_y, aes(x = x, y = y, xend = proj_x, yend = proj_y),
          col = "orange",
          arrow = arrow(length = unit(0.03, "npc"))
        )
      path_plot_y <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-y.png"
      )
      cowplot::ggsave2(
        path_plot_y, p_y,
        width = 5, height = 5, dpi = 300
      )
      # projection onto eigen
      p_eigen <- p_orig +
        geom_hline(yintercept = 0, size = 1) +
        geom_abline(
          slope = eigen_vector[2] / eigen_vector[1], intercept = 0,
          col = "gray65", size = 1
        ) +
        geom_segment(
          data = plot_tbl_eigen,
          aes(x = x, y = y, xend = proj_x, yend = proj_y),
          col = "red",
          arrow = arrow(length = unit(0.03, "npc"))
        )
      path_plot_eigen <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-eigen.png"
      )
      cowplot::ggsave2(
        path_plot_eigen, p_eigen,
        width = 5, height = 5, dpi = 300
      )

      # projected points
      # --------------------
      p_proj_init <- ggplot() +
        cowplot::theme_cowplot(font_size = 29) +
        cowplot::background_grid(major = "xy") +
        geom_vline(xintercept = 0, size = 1, col = "gray65") +
        geom_hline(yintercept = 0, size = 1, col = "gray65") +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
        ) +
        coord_equal() +
        lims(x = lim_vec, y = lim_vec)

      # projection onto x
      p_proj_x <- p_proj_init +
        geom_hline(yintercept = 0, size = 1) +
        geom_point(
          data = plot_tbl_x, aes(x = proj_x, y = proj_y),
          col = "dodgerblue"
        )
      path_plot_proj_x <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-proj_x.png"
      )
      cowplot::ggsave2(
        path_plot_proj_x, p_proj_x,
        width = 5, height = 5, dpi = 300
      )
      # p

      # histograms of scores
      # --------------------------------

      plot_tbl_len <- plot_tbl_orig |>
        dplyr::mutate(
          len = calc_proj_length(x, y, c(1, 1)),
          type = "proj_x"
        ) |>
        dplyr::bind_rows(
          plot_tbl_orig |>
            dplyr::mutate(
              len = calc_proj_length(x, y, c(0, 1)),
              type = "proj_y"
            )
        ) |>
        dplyr::bind_rows(
          plot_tbl_orig |>
            dplyr::mutate(
              len = calc_proj_length(x, y, eigen_vector),
              type = "proj_eigen"
            )
        )
      lim_vec_line <- range(plot_tbl_len$len)
      p_hist_x <- ggplot(
        plot_tbl_len |>
          dplyr::filter(type == "proj_x")
      ) +
        cowplot::theme_cowplot(font_size = 29) +
        cowplot::background_grid(major = "xy") +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
        ) +
        geom_histogram(
          aes(x = len, fill = type),
          bins = 7, alpha = 0.5, color = "dodgerblue",
          show.legend = FALSE, fill = "dodgerblue"
        ) +
        labs(y = "Count", x = "Projection length") +
        geom_rug() +
        lims(x = lim_vec_line)
      path_plot_x <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-x-line.png"
      )
      cowplot::ggsave2(
        path_plot_x, p_hist_x,
        width = 5, height = 5, dpi = 300
      )
      p_hist_y <- ggplot(
        plot_tbl_len |>
          dplyr::filter(type == "proj_y")
      ) +
        cowplot::theme_cowplot(font_size = 29) +
        cowplot::background_grid(major = "xy") +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
        ) +
        geom_histogram(
          aes(x = len, fill = type),
          bins = 7, alpha = 0.5, color = "orange",
          show.legend = FALSE, fill = "orange"
        ) +
        labs(y = "Count", x = "Projection length") +
        geom_rug() +
        lims(x = lim_vec_line)
      path_plot_y <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-y-line.png"
      )
      cowplot::ggsave2(
        path_plot_y, p_hist_y,
        width = 5, height = 5, dpi = 300
      )
      p_hist_eigen <- ggplot(
        plot_tbl_len |>
          dplyr::filter(type == "proj_eigen")
      ) +
        geom_histogram(
          aes(x = len, fill = type),
          bins = 7, alpha = 0.5, color = "red",
          show.legend = FALSE, fill = "red"
        ) +
        cowplot::theme_cowplot(font_size = 29) +
        cowplot::background_grid(major = "xy") +
        theme(
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")
        ) +
        labs(y = "Count", x = "Projection length") +
        geom_rug() +
        lims(x = lim_vec_line)
      path_plot_eigen <- projr::projr_path_get(
        "cache", "fig", "intro", "p-intro-eigen-line.png"
      )
      cowplot::ggsave2(
        path_plot_eigen, p_hist_eigen,
        width = 5, height = 5, dpi = 300
      )

      # plot progression
      # -------------------------
      p_intro_progression <- cowplot::plot_grid(
        p_orig, p_x, p_proj_x, p_hist_x,
        ncol = 2
      ) +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
        )
      path_grid_progression <- projr_path_get(
        "project",
        dir_cache, "fig", "intro", "p-intro-progression"
      )
      UtilsGGSV::ggsave2(
        path_grid_progression,
        p_intro_progression,
        dpi = 300, width = 27, height = 17
      )

      # plot comparisons
      # -------------------------
      p_intro_comparison <- cowplot::plot_grid(
        p_x, p_y, p_eigen,
        p_hist_x, p_hist_y, p_hist_eigen,
        nrow = 2, ncol = 3, rel_heights = c(1, 1)
      ) +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
        )

      path_grid_comparison <- projr_path_get(
        "project",
        dir_cache, "fig", "intro", "p-intro-comparison"
      )
      UtilsGGSV::ggsave2(
        path_grid_comparison,
        p_intro_comparison,
        dpi = 300, width = 27, height = 17
      )

      c(
        "grid-progression" = fs::path_rel(
          path_grid_progression, dir_cache
        ) |>
          as.character(),
        "grid-comparison" = fs::path_rel(
          path_grid_comparison, dir_cache
        ) |>
          as.character()
      )
    }
  ),
  tar_target(
    path_p_intro_docs,
    {
      path_grid_progression_vec_cache <- projr_path_get(
        "cache", path_p_intro_cache[["grid-progression"]]
      ) |>
        paste0( # nolint
          c(".png", ".pdf")
        )
      path_grid_progression_vec_docs_base <- projr_path_get(
        "project",
        dir_docs, "fig", "intro", "p-intro-progression"
      )
      path_grid_progression_vec_docs <- paste0(
        path_grid_progression_vec_docs_base, c(".png", "pdf")
      )
      for (i in seq_along(path_grid_progression_vec_docs)) {
        path_docs <- path_grid_progression_vec_docs[i]
        if (file.exists(path_docs)) {
          invisible(file.remove(path_docs))
        }
        path_cache <- path_grid_progression_vec_cache[i]
        if (!file.exists(path_docs)) {
          invisible(file.copy(
            path_cache,
            path_docs
          ))
        }
      }

      path_grid_comparison_vec_cache <- projr_path_get(
        "cache", path_p_intro_cache[["grid-comparison"]]
      ) |>
        paste0( # nolint
          c(".png", ".pdf")
        )
      path_grid_comparison_vec_docs_base <- projr_path_get(
        "project",
        dir_docs, "fig", "intro", "p-intro-comparison"
      )
      path_grid_comparison_vec_docs <- paste0(
        path_grid_comparison_vec_docs_base, c(".png", "pdf")
      )
      for (i in seq_along(path_grid_comparison_vec_docs)) {
        path_docs <- path_grid_comparison_vec_docs[i]
        if (file.exists(path_docs)) {
          invisible(file.remove(path_docs))
        }
        path_cache <- path_grid_comparison_vec_cache[i]
        if (!file.exists(path_docs)) {
          invisible(file.copy(
            path_cache,
            path_docs
          ))
        }
      }
    },
    cue = tar_cue("always")
  )
)
