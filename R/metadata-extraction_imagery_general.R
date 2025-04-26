# Read in the EXIF data from a set of absolute image paths and drop the ThumbnailImage and
# PreviewImage attributes if they exist, and convert all cols to character
#' @export
read_exif_drop_thumbnails = function(image_paths) {

  # These are all the tags we've enocuntered in EXIF at OFO through 2025. Mostly this is so that we
  # can exclude the few that we don't want, particularly the thumbnail and preview images, but
  # read_exif does not have a way to specify specific tags to exclude, so we have to specify all to read.
  focal_tags = c("SourceFile", "ExifToolVersion", "FileName", "Directory", "FileSize", 
  "FileModifyDate", "FileAccessDate", "FileInodeChangeDate", "FilePermissions", 
  "FileType", "FileTypeExtension", "MIMEType", "ExifByteOrder", 
  "ImageDescription", "Model", "Orientation", "XResolution", "YResolution", 
  "ResolutionUnit", "Software", "ModifyDate", "YCbCrPositioning", 
  "ExposureTime", "FNumber", "ExposureProgram", "ISO", "ExifVersion", 
  "DateTimeOriginal", "CreateDate", "ComponentsConfiguration", 
  "CompressedBitsPerPixel", "ShutterSpeedValue", "ApertureValue", 
  "ExposureCompensation", "MaxApertureValue", "SubjectDistance", 
  "MeteringMode", "LightSource", "Flash", "FocalLength", "Warning", 
  "Make", "SpeedX", "SpeedY", "SpeedZ", "Pitch", "Yaw", "Roll", 
  "CameraPitch", "CameraYaw", "CameraRoll", "FlashpixVersion", 
  "ColorSpace", "ExifImageWidth", "ExifImageHeight", "InteropIndex", 
  "InteropVersion", "ExposureIndex", "FileSource", "SceneType", 
  "CustomRendered", "ExposureMode", "WhiteBalance", "DigitalZoomRatio", 
  "FocalLengthIn35mmFormat", "SceneCaptureType", "GainControl", 
  "Contrast", "Saturation", "Sharpness", "SubjectDistanceRange", 
  "SerialNumber", "GPSVersionID", "GPSLatitudeRef", "GPSLongitudeRef", 
  "GPSAltitudeRef", "XPComment", "XPKeywords", "Compression", "ThumbnailOffset", 
  "ThumbnailLength", "About", "Format", "AbsoluteAltitude", "RelativeAltitude", 
  "GimbalRollDegree", "GimbalYawDegree", "GimbalPitchDegree", "FlightRollDegree", 
  "FlightYawDegree", "FlightPitchDegree", "CamReverse", "GimbalReverse", 
  "SelfData", "Version", "HasSettings", "HasCrop", "AlreadyApplied", 
  "MPFVersion", "NumberOfImages", "MPImageFlags", "MPImageFormat", 
  "MPImageType", "MPImageLength", "MPImageStart", "DependentImage1EntryNumber", 
  "DependentImage2EntryNumber", "ImageUIDList", "TotalFrames", 
  "ImageWidth", "ImageHeight", "EncodingProcess", "BitsPerSample", 
  "ColorComponents", "YCbCrSubSampling", "Aperture", "ImageSize", 
  "Megapixels", "ScaleFactor35efl", "ShutterSpeed", "GPSAltitude", 
  "GPSLatitude", "GPSLongitude", "CircleOfConfusion", "FOV", "FocalLength35efl", 
  "GPSPosition", "HyperfocalDistance", "LightValue", "mission_id", 
  "sub_mission_id", "GPSLongtitude", "FlightXSpeed", "FlightYSpeed", 
  "FlightZSpeed", "CalibratedFocalLength", "CalibratedOpticalCenterX", 
  "CalibratedOpticalCenterY", "RtkFlag", "ProcessingSoftware", 
  "SubSecTimeOriginal", "FocalPlaneXResolution", "FocalPlaneYResolution", 
  "FocalPlaneResolutionUnit", "LensModel", "GPSTimeStamp", "GPSStatus", 
  "GPSMapDatum", "GPSDateStamp", "UniqueCameraModel", "CameraSerialNumber", 
  "ImageNumber", "XMPToolkit", "CamId", "IMULinearVelocity", "GPSXYAccuracy", 
  "GPSZAccuracy", "IMUPitchAccuracy", "IMURollAccuracy", "IMUYawAccuracy", 
  "FlightUUID", "Manufacturer", "SwVersion", "SubSecDateTimeOriginal", 
  "GPSDateTime", "DOF", "FlashEnergy", "LensMake", "RtkStdLon", 
  "RtkStdLat", "RtkStdHgt", "DewarpFlag", "LensID", "DeviceSettingDescription", 
  "PhotoDiff", "submission_id", "DewarpData", "SensitivityType", 
  "LensInfo", "GpsStatus", "AltitudeType", "RtkDiffAge", "SurveyingMode", 
  "UTCAtExposure", "ShutterType", "ShutterCount", "DroneModel", 
  "DroneSerialNumber", "CaptureUUID", "JFIFVersion", "RigName", 
  "RigCameraIndex", "FocusDistance", "PictureQuality", "SensingMethod", 
  "LensSerialNumber", "AboveGroundAltitude", "ModelType", "PrincipalPoint", 
  "PerspectiveFocalLength", "PerspectiveDistortion", "HorizCS", 
  "VertCS", "Comment", "ImageSource", "NTRIPMountPoint", "NTRIPPort", 
  "NTRIPHost")

  exif = exifr::read_exif(image_paths, tags = focal_tags)

  exif = exif |>
    # Remove thumbnail data
    dplyr::select(-dplyr::any_of(c("ThumbnailImage", "PreviewImage"))) |>
    # Convert all cols to character
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    # Make sure SourceFile is in R-friendly format
    dplyr::mutate(SourceFile = image_paths)
}
