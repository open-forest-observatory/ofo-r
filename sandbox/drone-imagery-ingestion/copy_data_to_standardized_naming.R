library("R.utils")
library("stringr")

folder_names = c(
"BORR/2020-02-20_1020_FullReserve_Soda3D",
"BORR/2020-02-20_1530_FullReserve_AeriaX",
"BORR/2020-02-21_1300_Arbor_X7",
"BORR/2020-05-09_1350_Arbor_X7",
"BORR/2020-05-12_1315_Arbor_X7",
"BORR/2020-05-20_1100_Grove_X7",
"BORR/2020-05-21_1200_LowerWindmill_X7",
"BORR/2020-05-21_1200_UpperWindmill_X7",
"BORR/2020-05-22_1440_Arbor_X7",
"BORR/2020-05-22_HHMM_Windmill_X7",
"BORR/2020-05-25_1300_BullPen_X7",
"BORR/2020-06-02_hhmm_Windmill_X7",
"BORR/2020-06-05_1320_ArborLG_X7",
"BORR/2020-06-05_1410_ArborLG_X7",
"BORR/2020-09-22_1130_ArborPostBurn_X7",
"BORR/2020-09-22_1400_ArborHillSlope_X7",
"BORR/2020-10-26_1400_FullReservePt1_AeriaX",
"BORR/2020-10-27_0930_FullReservePt2_AeriaX",
"BORR/2020-10-27_1200_FullReservePt3_Soda3D",
"BORR/2020-10-28_1030_FullReservePt4_Soda3D",
"BORR/2020-10-29_1200_Arbor_X7",
"BORR/2020-10-31_HHMM_OakWoodlandUnburned1_X7",
"BORR/2020-10-31_HHMM_OakWoodlandUnburned2 _X7",
"BORR/2020-10-31_HHMM_PlantPlot2_2_X7",
"BORR/2020-10-31_HHMM_PlantPlot1_X7",
"BORR/2020-10-31_HHMM_PlantPlot2 _X7",
"BORR/2020-10-31_HHMM_PlantPlot3_X7",
"BORR/2020-10-31_HHMM_PlantPlot4_X7",
"BORR/2020-10-31_HHMM_ScrubBurn2_X7",
"BORR/2020-10-31_HHMM_ScrubBurned1_X7",
"BORR/2020-10-31_HHMM_ScrubUnburned1_X7",
"BORR/2020-10-31_HHMM_ScrubUnburnt2_X7",
"HAST/2020-09-25_1300_UpperT_X7",
"HAST/2020-09-26_0930_LowerT_X7",
"HAST/2020-09-26_1600_UpperT_X7",
"HAST/2020-09-27-0945_UpperMain_X7",
"QUAIL/2020-02-11_1300_Oaks_X7",
"QUAIL/2020-02-12_1215_Woodrat_X7",
"QUAIL/2020-02-12_1500_Oaks_X7",
"QUAIL/2020-11-16_1100_Grass1_X3",
"QUAIL/2020-11-16_1145_OakChaparral1_X3",
"QUAIL/2020-11-16_1230_Grass2_X3",
"QUAIL/2020-11-16_1320_OakChaparral2_X3",
"QUAIL/2020-11-19_1105_GrassOakChaparral3_X3",
"QUAIL/2020-11-19_1230_OakChaparral4_X3",
"QUAIL/2020-12-02_1215_Grass4_X3")

IDs = c(479,
544,
545,
546,
547,
548,
549,
550,
551,
552,
553,
554,
555,
556,
557,
558,
559,
560,
561,
562,
563,
564,
565,
566,
567,
568,
569,
570,
571,
572,
573,
574,
575,
576,
577,
578,
579,
580,
581,
582,
583,
584,
585,
586,
587,
588)

INPUT_FOLDER = "/ofo-share/NRS-VEG-data/raw_imagery/2020/original"
OUTPUT_FOLDER = "/ofo-share/drone-imagery-organization/1_manually-cleaned/2020-ucnrs"

combined = data.frame(folder = folder_names, ID = IDs)

for(i in 1:nrow(combined)) {
    row <- combined[i,]
    folder = row[["folder"]]
    ID = row[["ID"]]

    padded_ID = str_pad(ID, 6, pad="0")

    input_folder = paste0(INPUT_FOLDER, "/", folder)
    output_folder = paste0(OUTPUT_FOLDER, "/", padded_ID)
    print(input_folder)
    print(output_folder)
    copyDirectory(input_folder, output_folder)
    # do stuff with row
}