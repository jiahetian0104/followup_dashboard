import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { Presentation, PresentationFile } from "@oai/artifact-tool";

const SKILL_DIR = "/Users/tianjiah/.codex/plugins/cache/openai-primary-runtime/presentations/26.909.12148/skills/presentations";
const workspaceDir = "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard";
const TMP_DIR = path.join(workspaceDir, ".codex_ppt_build");
const FINAL_PPTX = path.join(workspaceDir, "presentations_output", "IPA_Assignment_and_Task_Eligibility_Logic.pptx");
const RUNTIME_PYTHON = "/Users/tianjiah/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3";

const { resolvePresentationFont, finalizePresentation } = await import(
  pathToFileURL(path.join(SKILL_DIR, "container_tools/artifact_tool_utils.mjs")).href
);

await fs.mkdir(TMP_DIR, { recursive: true });
await fs.mkdir(path.dirname(FINAL_PPTX), { recursive: true });

const font = resolvePresentationFont();
const pres = Presentation.create({ slideSize: { width: 1280, height: 720 } });

const C = {
  ink: "#17324D",
  teal: "#1F7A7A",
  tealLight: "#D9EEEE",
  blue: "#3C73A8",
  blueLight: "#E4EEF7",
  orange: "#D8752B",
  orangeLight: "#F8E8D8",
  green: "#3E8A65",
  greenLight: "#E2F0E8",
  gray: "#657584",
  grayLight: "#EEF2F5",
  line: "#C8D2DB",
  white: "#FFFFFF",
};

function addText(slide, text, left, top, width, height, opts = {}) {
  const box = slide.shapes.add({
    geometry: "textbox",
    position: { left, top, width, height },
    fill: opts.fill ?? "none",
    line: opts.line ?? { fill: "none", width: 0 },
    borderRadius: opts.borderRadius,
  });
  box.text = text;
  box.text.style = {
    typeface: font,
    fontSize: opts.fontSize ?? 24,
    bold: opts.bold ?? false,
    color: opts.color ?? C.ink,
    autoFit: opts.autoFit ?? "shrinkText",
    alignment: opts.alignment ?? "left",
  };
  return box;
}

function addRect(slide, left, top, width, height, fill, line = C.line, radius = 16) {
  return slide.shapes.add({
    geometry: "rect",
    position: { left, top, width, height },
    fill,
    line: { style: "solid", fill: line, width: 1 },
    borderRadius: radius,
  });
}

function addTitle(slide, title, subtitle = "") {
  addText(slide, title, 68, 40, 1144, 56, { fontSize: 44, bold: true });
  if (subtitle) addText(slide, subtitle, 70, 101, 1110, 34, { fontSize: 21, color: C.gray });
  slide.shapes.add({
    geometry: "line",
    position: { left: 70, top: 142, width: 1140, height: 0 },
    fill: "none",
    line: { style: "solid", fill: C.line, width: 1 },
  });
}

function addFooter(slide, n) {
  addText(slide, String(n), 1190, 680, 28, 18, { fontSize: 14, color: C.gray, alignment: "right" });
}

// Slide 1
{
  const s = pres.slides.add();
  s.background.fill = C.white;
  s.shapes.add({
    geometry: "rect",
    position: { left: 0, top: 0, width: 1280, height: 720 },
    fill: C.ink,
    line: { fill: "none", width: 0 },
  });
  s.shapes.add({
    geometry: "rect",
    position: { left: 0, top: 0, width: 22, height: 720 },
    fill: C.teal,
    line: { fill: "none", width: 0 },
  });
  addText(s, "IPA Dashboard Logic", 94, 205, 1040, 86, { fontSize: 58, bold: true, color: C.white });
  addText(s, "FTM assignment and task eligibility", 98, 304, 900, 44, { fontSize: 29, color: "#D6E2EC" });
  addText(s, "CHARM follow-up dashboard", 98, 570, 600, 30, { fontSize: 19, color: "#AFC3D3" });
  s.speakerNotes.textFrame.setText("Source: Internal IPA dashboard methodology implemented in IPA Data.R, refreshed September 20, 2026.");
}

// Slide 2
{
  const s = pres.slides.add();
  s.background.fill = C.white;
  addTitle(s, "Task eligibility", "Only participants in an active IPA age band enter task counts and progress denominators");

  addText(s, "Visible in roster only", 76, 190, 235, 34, { fontSize: 24, bold: true, color: C.gray });
  addText(s, "6–11 months", 76, 248, 225, 62, { fontSize: 26, bold: true, fill: C.grayLight, borderRadius: 12, alignment: "center" });
  addText(s, "Potential participants", 76, 328, 225, 62, { fontSize: 24, bold: true, fill: C.grayLight, borderRadius: 12, alignment: "center" });
  addText(s, "These participants remain filterable, but they do not add tasks to the denominator.", 76, 414, 235, 105, { fontSize: 20, color: C.gray });

  addText(s, "Task eligible", 370, 190, 760, 34, { fontSize: 24, bold: true, color: C.teal });
  const bands = ["12–23\nmonths", "24–35\nmonths", "3–5\nyears", "6–10\nyears", "11–17\nyears", "18–20\nyears"];
  bands.forEach((label, i) => {
    const x = 370 + i * 133;
    addText(s, label, x, 248, 116, 84, { fontSize: 22, bold: true, fill: i < 2 ? C.blueLight : C.tealLight, borderRadius: 12, alignment: "center" });
    if (i < bands.length - 1) {
      s.shapes.add({ geometry: "line", position: { left: x + 116, top: 290, width: 17, height: 0 }, fill: "none", line: { style: "solid", fill: C.line, width: 2 } });
    }
  });
  addText(s, "Task rows are created from the participant's current statusId age band.", 370, 365, 790, 42, { fontSize: 23, color: C.ink });
  addText(s, "Withdrawn and ECHO 2 Refusal records are excluded before the roster is built.", 370, 432, 790, 54, { fontSize: 21, color: C.gray });
  addText(s, "Denominator", 370, 535, 150, 32, { fontSize: 21, bold: true, color: C.orange });
  addText(s, "One row for every applicable participant × task × age band", 520, 526, 620, 52, { fontSize: 25, bold: true, fill: C.orangeLight, borderRadius: 12, alignment: "center" });
  addFooter(s, 2);
  s.speakerNotes.textFrame.setText("Source: participant roster and task eligibility logic in IPA Data.R. Potential participants and 6–11 month participants remain visible but do not enter task denominators.");
}

// Slide 3
{
  const s = pres.slides.add();
  s.background.fill = C.white;
  addTitle(s, "Calendly FTM assignment", "Calendly host or inviter is the authoritative source for IPA responsibility");

  const start = addRect(s, 70, 245, 220, 110, C.blueLight, C.blue);
  addText(s, "Participant + current\nIPA age band", 85, 267, 190, 66, { fontSize: 25, bold: true, alignment: "center" });
  const exact = addRect(s, 370, 190, 260, 110, C.greenLight, C.green);
  addText(s, "Same age band\nCalendly FTM found", 390, 212, 220, 64, { fontSize: 24, bold: true, alignment: "center" });
  const fallback = addRect(s, 370, 385, 260, 125, C.orangeLight, C.orange);
  addText(s, "No same-band match\nCheck the immediately\nprevious age band", 390, 401, 220, 91, { fontSize: 22, bold: true, alignment: "center" });
  const assigned = addRect(s, 760, 190, 350, 110, C.greenLight, C.green);
  addText(s, "Assigned\nSame age band", 790, 214, 290, 62, { fontSize: 27, bold: true, alignment: "center" });
  const fallbackAssigned = addRect(s, 760, 385, 350, 125, C.orangeLight, C.orange);
  addText(s, "Assigned\nPrevious age band fallback", 790, 410, 290, 76, { fontSize: 25, bold: true, alignment: "center" });
  const unassigned = addRect(s, 760, 565, 350, 80, C.grayLight, C.gray);
  addText(s, "No match: Unassigned", 790, 586, 290, 38, { fontSize: 26, bold: true, alignment: "center", color: C.gray });

  s.shapes.connect(start, exact, { kind: "elbow", fromSide: "right", toSide: "left", line: { style: "solid", fill: C.blue, width: 2 }, tail: { type: "arrow", width: "med", length: "med" } });
  s.shapes.connect(start, fallback, { kind: "elbow", fromSide: "right", toSide: "left", line: { style: "solid", fill: C.orange, width: 2 }, tail: { type: "arrow", width: "med", length: "med" } });
  s.shapes.connect(exact, assigned, { kind: "straight", fromSide: "right", toSide: "left", line: { style: "solid", fill: C.green, width: 2 }, tail: { type: "arrow", width: "med", length: "med" } });
  s.shapes.connect(fallback, fallbackAssigned, { kind: "straight", fromSide: "right", toSide: "left", line: { style: "solid", fill: C.orange, width: 2 }, tail: { type: "arrow", width: "med", length: "med" } });
  s.shapes.connect(fallbackAssigned, unassigned, { kind: "elbow", fromSide: "bottom", toSide: "top", line: { style: "dashed", fill: C.gray, width: 2 }, tail: { type: "arrow", width: "med", length: "med" } });

  addText(s, "YES", 664, 213, 65, 28, { fontSize: 17, bold: true, color: C.green, alignment: "center" });
  addText(s, "NO", 300, 373, 56, 28, { fontSize: 17, bold: true, color: C.orange, alignment: "center" });
  addText(s, "NO", 885, 525, 60, 25, { fontSize: 17, bold: true, color: C.gray, alignment: "center" });
  addFooter(s, 3);
  s.speakerNotes.textFrame.setText("Source: participant Calendly assignment lookup in IPA Data.R. Ripple and Call List FTM fields are excluded from IPA credit assignment.");
}

// Slide 4
{
  const s = pres.slides.add();
  s.background.fill = C.white;
  addTitle(s, "Previous age band lookup", "Fallback uses one adjacent age band only");
  const values = [
    ["Current IPA age band", "Previous age band checked", "Fallback available"],
    ["6–11 months", "None", "No"],
    ["12–23 months", "6–11 months", "Yes"],
    ["24–35 months", "12–23 months", "Yes"],
    ["3–5 years", "24–35 months", "Yes"],
    ["6–10 years", "3–5 years", "Yes"],
    ["11–17 years", "6–10 years", "Yes"],
    ["18–20 years", "11–17 years", "Yes"],
  ];
  const table = s.tables.add({ rows: values.length, columns: 3, left: 145, top: 185, width: 990, height: 420, values, columnWidths: [350, 390, 250] });
  table.borders.assign({ style: "solid", fill: C.line, width: 1 });
  table.cells.block({ row: 0, column: 0, rowCount: 1, columnCount: 3 }).assign({ fill: C.ink, textStyle: { typeface: font, fontSize: 21, bold: true, color: C.white }, margins: { left: 12, right: 12, top: 8, bottom: 8 } });
  table.cells.block({ row: 1, column: 0, rowCount: 7, columnCount: 3 }).assign({ textStyle: { typeface: font, fontSize: 20, color: C.ink }, margins: { left: 12, right: 12, top: 7, bottom: 7 } });
  for (let r = 1; r < values.length; r++) {
    table.cells.block({ row: r, column: 0, rowCount: 1, columnCount: 3 }).fill = r % 2 === 0 ? "#F6F8FA" : C.white;
  }
  table.getCell(1, 2).fill = C.grayLight;
  table.getCell(1, 2).text.style = { typeface: font, fontSize: 20, bold: true, color: C.gray };
  for (let r = 2; r < values.length; r++) {
    table.getCell(r, 2).fill = C.orangeLight;
    table.getCell(r, 2).text.style = { typeface: font, fontSize: 20, bold: true, color: C.orange };
  }
  addText(s, "A 6–10 year participant can use a 3–5 year Calendly FTM, but never a 24–35 month FTM.", 180, 625, 920, 42, { fontSize: 22, bold: true, color: C.orange, alignment: "center" });
  addFooter(s, 4);
  s.speakerNotes.textFrame.setText("Source: ipa_previous_age_band_map in IPA Data.R. The fallback never skips an age band.");
}

// Slide 5
{
  const s = pres.slides.add();
  s.background.fill = C.white;
  addTitle(s, "Task responsibility and history", "Credit follows the FTM responsible for that participant's task and age band");

  addText(s, "Earlier age band", 90, 190, 245, 34, { fontSize: 23, bold: true, color: C.blue });
  const prior = addRect(s, 90, 245, 310, 150, C.blueLight, C.blue);
  addText(s, "FTM A", 120, 270, 250, 42, { fontSize: 31, bold: true, alignment: "center" });
  addText(s, "Complete, No-Show, Incomplete,\nand No record tasks stay here", 120, 324, 250, 54, { fontSize: 20, color: C.gray, alignment: "center" });

  addText(s, "Later age band", 485, 190, 245, 34, { fontSize: 23, bold: true, color: C.teal });
  const later = addRect(s, 485, 245, 310, 150, C.tealLight, C.teal);
  addText(s, "FTM B", 515, 270, 250, 42, { fontSize: 31, bold: true, alignment: "center" });
  addText(s, "New age-band tasks belong to\nthe later responsible FTM", 515, 324, 250, 54, { fontSize: 20, color: C.gray, alignment: "center" });
  s.shapes.connect(prior, later, { kind: "straight", fromSide: "right", toSide: "left", line: { style: "solid", fill: C.line, width: 3 }, tail: { type: "arrow", width: "med", length: "med" } });

  addText(s, "Dashboard fields", 875, 190, 285, 34, { fontSize: 23, bold: true, color: C.ink });
  addText(s, "Current FTM", 875, 246, 280, 42, { fontSize: 23, bold: true, fill: C.grayLight, borderRadius: 10 });
  addText(s, "Responsible FTM", 875, 305, 280, 42, { fontSize: 23, bold: true, fill: C.grayLight, borderRadius: 10 });
  addText(s, "Assignment Rule", 875, 364, 280, 42, { fontSize: 23, bold: true, fill: C.grayLight, borderRadius: 10 });
  addText(s, "Matched Age Band", 875, 423, 280, 42, { fontSize: 23, bold: true, fill: C.grayLight, borderRadius: 10 });

  addText(s, "Fallback assignments remain clearly labeled. A direct same-age Calendly match can replace a provisional fallback when it becomes available.", 90, 495, 705, 92, { fontSize: 23, bold: true, fill: C.orangeLight, borderRadius: 12 });
  addText(s, "FTMs can filter by staff, task, age group, participant scope, outcome, and data snapshot.", 90, 615, 1065, 36, { fontSize: 22, color: C.gray });
  addFooter(s, 5);
  s.speakerNotes.textFrame.setText("Source: FTM assignment history and task responsibility ledger logic in IPA Data.R, plus dashboard field definitions in streamlit_app.py.");
}

const requirements = {
  explicitTotalSlideCount: 5,
  requiredNativeTableOwnerSlides: [4],
  requiredNativeChartOwnerSlides: [],
};
const fontPolicy = { basis: "design", families: [font] };
const stagingDir = path.join(workspaceDir, ".codex-finalizer");
await fs.mkdir(stagingDir, { recursive: true });
const candidatePath = path.join(stagingDir, "ipa_logic_candidate.pptx");
await (await PresentationFile.exportPptx(pres)).save(candidatePath);

const result = await finalizePresentation({
  ...requirements,
  workspaceDir,
  candidatePath,
  finalPath: FINAL_PPTX,
  pythonExecutable: RUNTIME_PYTHON,
  integrityValidatorPath: path.join(SKILL_DIR, "container_tools/inspect_presentation_package_integrity.py"),
  layoutValidatorPath: path.join(SKILL_DIR, "container_tools/inspect_presentation_layout_geometry.py"),
  layoutArgs: [
    "--expected-slide-size-emu", "12192000,6858000",
    "--validate-bullet-geometry",
    "--validate-heading-fit",
    "--require-native-table-slide", "4"
  ],
  requiredNativeTableOwnerSlides: [4],
  fontPolicy,
  verifyArtifactToolImport: true,
  receiptPath: path.join(stagingDir, "IPA_Assignment_and_Task_Eligibility_Logic.validation.json"),
});

console.log(JSON.stringify({ finalPath: FINAL_PPTX, font, result }, null, 2));
