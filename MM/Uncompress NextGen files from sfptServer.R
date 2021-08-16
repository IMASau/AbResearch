## Uncompress NextGen files from sfptServer ####

## Choose directory where compressed files for import are located
imp_dir <- 'C:/CloudStor/R_Stuff/MMLF/MM_sftp_rawtestdata'

## Choose directory where unpacked .txt files are to be saved
out_dir <- 'C:/CloudStor/R_Stuff/MMLF/MM_sftp_rawtestdata'

## UnPack compressed files (tar.gz) ####

# Get a full list of all compressed files in the import directory
dirlist_cmp <- list_files_with_exts(imp_dir,c("tar.gz"), full.names=FALSE)

# Get a full list of all unpacked .txt files in the export directory
dirlist_txt <- list_files_with_exts(out_dir,c("txt"), full.names=FALSE)

## Convert both lists to data frames and combine
dirlistdf_cmp <- as.data.frame(dirlist_cmp)
dirlistdf_cmp$dirlist_cmp <- as.character(dirlistdf_cmp$dirlist_cmp)
dirlistdf_cmp <- dirlistdf_cmp %>%
  dplyr::rename(FileName = dirlist_cmp)
dirlistdf_txt <- as.data.frame(dirlist_txt)
dirlistdf_txt$dirlist_txt <- as.character(dirlistdf_txt$dirlist_txt)
dirlistdf_txt <- dirlistdf_txt %>%
  dplyr::rename(FileName = dirlist_txt)

dirlistdf_unp <- rbind(dirlistdf_cmp,dirlistdf_txt)

# Find file names and extensions
dirlistdf_unp <- dirlistdf_unp %>% separate(FileName,c("F_name","F_ext"),sep="[.]",remove=F)

#Remove unpacked files from list (ie. find duplicates)
unpacked_files <- dirlistdf_unp[duplicated(dirlistdf_unp[,c("F_name")]),]
files_to_unpack <- dirlistdf_unp %>%
 filter(!F_name %in% c(unpacked_files$F_name)) %>%
 filter(F_ext == c("tar"))

# Unpack files
tic()
numfiles <- nrow(files_to_unpack)
if (numfiles > 0){
 for (f in 1:numfiles){
  untar(paste(imp_dir,"\\",files_to_unpack$FileName[f],sep=""),exdir = out_dir)
 }}
toc()

