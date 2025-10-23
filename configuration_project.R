install.packages(c("usethis","gitcreds","gh","gert"))

gh::gh_whoami()            # should show your GitHub user
usethis::git_sitrep()      # should no longer show the 401
usethis::edit_r_environ()

## Para hacer primer commit y crear repo

usethis::use_git()

# 2) Make the first commit (creates the first branch)
gert::git_add(".")
gert::git_commit("Initial Quarto site")

usethis::use_github()  # creates repo, sets remote, pushes

## Instalar quarto add quarto-ext/fontawesome
