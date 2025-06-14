% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hpoptimize.R
\name{cleanup_hpo_runs}
\alias{cleanup_hpo_runs}
\title{Clean Up Old HPO Runs}
\usage{
cleanup_hpo_runs(
  runs_dir = "runs/hpo",
  keep_n = 10,
  keep_best = 5,
  remove_incomplete = TRUE,
  dry_run = FALSE
)
}
\arguments{
\item{runs_dir}{Base directory containing runs (default: "runs/hpo")}

\item{keep_n}{Number of most recent completed runs to keep (default: 10)}

\item{keep_best}{Number of best-scoring runs to keep (default: 5)}

\item{remove_incomplete}{Whether to remove incomplete runs (default: TRUE)}

\item{dry_run}{If TRUE, only shows what would be deleted without actually deleting}
}
\description{
Removes old or incomplete HPO runs, keeping only the most recent or best ones
}
\examples{
\dontrun{
# See what would be cleaned up
cleanup_hpo_runs(dry_run = TRUE)

# Actually clean up
cleanup_hpo_runs(keep_n = 5, keep_best = 3)
}
}
