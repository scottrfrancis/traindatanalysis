
# directory of TCX file repo
#dropbox.root<- "~/scofra01/Dropbox"
dropbox.root<- "~/Dropbox"
repo.dir<- paste( dropbox.root, "Apps", "tapiriik", sep="/" )

repo.files<- list.files( repo.dir, pattern="*.segments.csv", full.names=TRUE )
