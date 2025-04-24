# This is a command line call to upload processed imagery to CyVerse. It assumes you have configured
# rclone with a remote called cyverse, as described in the ofo-credentials doc

# Within the cyverse folder ofo/public/missions/<mission_id>, this will upload:
# image metadata (points)
# images (zipped)
# mission metadata (p)olygon)

rclone copy /ofo-share/drone-data-publish/02 cyverse:iplant/projects/ofo/public/missions/ --progress
--transfers 5 --include "image-metadata/**" --include "images/**" --include "**mission-metadata/**"