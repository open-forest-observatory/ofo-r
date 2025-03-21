# This is to run enough of the pipline to update the metadata for already organized images. You need
# to set the project id in the file sandbox/drone-imagery-ingestion/imagery_project_name.txt first.

Rscript --vanilla /ofo-share/repos-derek/ofo-r/sandbox/drone-imagery-ingestion/10_merge-exif-and-baserow.R
Rscript --vanilla /ofo-share/repos-derek/ofo-r/sandbox/drone-imagery-ingestion/11_metadata-to-geospatial.R