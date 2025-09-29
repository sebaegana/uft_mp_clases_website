install.packages(c("usethis","gitcreds","gh","gert"))

gitcreds::gitcreds_set()   # paste the token when prompted

gh::gh_whoami()            # should show your GitHub user
usethis::git_sitrep()      # should no longer show the 401

usethis::edit_r_environ()

# 2) Make the first commit (creates the first branch)
gert::git_add(".")
gert::git_commit("Initial Quarto site")

usethis::use_github()  # creates repo, sets remote, pushes

## Instalar quarto add quarto-ext/fontawesome
