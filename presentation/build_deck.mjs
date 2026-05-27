import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";

const artifactRoot = "C:/Users/jerem/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/@oai/artifact-tool";
const artifact = await import(pathToFileURL(path.join(artifactRoot, "dist", "artifact_tool.mjs")).href);

const {
  Presentation,
  PresentationFile,
  column,
  row,
  layers,
  panel,
  text,
  image,
  shape,
  rule,
  fill,
  fixed,
  grow,
  hug,
  wrap,
} = artifact;

const repo = "C:/Users/jerem/Documents/GitHub/inflationinequality";
const outputDir = path.join(repo, "presentation", "output");
const previewDir = path.join(outputDir, "previews");
const figureDir = path.join(outputDir, "figures");
await mkdir(previewDir, { recursive: true });

const W = 1920;
const H = 1080;
const ink = "#15202B";
const muted = "#52616B";
const pale = "#F6F8F9";
const teal = "#1B998B";
const amber = "#F2A541";
const brick = "#D95D39";
const blue = "#3B5BA5";
const purple = "#7C3AED";
const repoUrl = "github.com/JeremiMontornes/inflationinequality";

const deck = Presentation.create({ slideSize: { width: W, height: H } });

function t(value, opts = {}) {
  return text(value, {
    width: opts.width ?? fill,
    height: opts.height ?? hug,
    style: {
      fontFace: "Aptos",
      fontSize: opts.size ?? 30,
      bold: opts.bold ?? false,
      color: opts.color ?? ink,
      ...opts.style,
    },
    name: opts.name,
  });
}

function bullets(items, opts = {}) {
  return column({ width: opts.width ?? fill, height: hug, gap: opts.gap ?? 14 }, items.map((item, i) =>
    row({ width: fill, height: hug, gap: 18, align: "start" }, [
      shape({ width: fixed(20), height: fixed(20), geometry: "ellipse", fill: opts.dot ?? teal, line: { color: opts.dot ?? teal, transparency: 100 } }),
      t(item, { width: fill, size: opts.size ?? 27, color: opts.color ?? ink, name: `bullet-${i + 1}` }),
    ])
  ));
}

function addSlide(root, background = "#FFFFFF") {
  const slide = deck.slides.add();
  slide.compose(
    layers({ width: fixed(W), height: fixed(H) }, [
      shape({ width: fixed(W), height: fixed(H), fill: background, line: { color: background, transparency: 100 } }),
      root,
    ]),
    { frame: { left: 0, top: 0, width: W, height: H }, baseUnit: 8 }
  );
  return slide;
}

function header(label, title, subtitle) {
  return column({ width: fill, height: hug, gap: 18 }, [
    t(label.toUpperCase(), { size: 18, bold: true, color: teal, style: { letterSpacing: 1 } }),
    t(title, { size: 50, bold: true, width: fixed(1700), color: ink }),
    subtitle ? t(subtitle, { size: 25, color: muted, width: wrap(1240) }) : t("", { size: 1 }),
  ]);
}

function chartSlide({ label, title, subtitle, chartFile, bulletsText, color, note }) {
  return addSlide(
    column({ width: fixed(W), height: fixed(H), padding: { left: 88, right: 88, top: 58, bottom: 46 }, gap: 28 }, [
      header(label, title, subtitle),
      row({ width: fill, height: grow(1), gap: 38, align: "center" }, [
        panel(
          { width: fixed(1160), height: fill, padding: { x: 0, y: 0 }, fill: "#FFFFFF", line: { color: "#DDE5EA", transparency: 0 }, borderRadius: 8 },
          image({
            path: path.join(figureDir, chartFile),
            width: fill,
            height: fill,
            fit: "contain",
            alt: title,
          })
        ),
        column({ width: fill, height: fill, justify: "center", gap: 34 }, [
          bullets(bulletsText, { size: 27, dot: color, gap: 22 }),
          t(note ?? "Lecture : chaque ligne montre l'inflation subie par un groupe de ménages.", { size: 19, color: "#6B7280", width: wrap(510) }),
        ]),
      ]),
      t(`Graphiques calculés avec inflationinequality - ${repoUrl}`, { size: 15, color: "#75828A" }),
    ])
  );
}

addSlide(
  layers({ width: fixed(W), height: fixed(H) }, [
    shape({ width: fixed(W), height: fixed(H), fill: "#FFFFFF", line: { color: "#FFFFFF", transparency: 100 } }),
    shape({ width: fixed(720), height: fixed(1080), fill: pale, line: { color: pale, transparency: 100 } }),
    shape({ width: fixed(18), height: fixed(760), fill: teal, line: { color: teal, transparency: 100 } }),
    column({ width: fixed(1260), height: fixed(900), padding: { left: 108, top: 92 }, gap: 34 }, [
      t("INFLATIONINEQUALITY", { size: 20, bold: true, color: teal }),
      t("Voir l'inflation que la moyenne ne montre pas", { size: 76, bold: true, width: wrap(1040) }),
      t("Un package R pour produire rapidement des datavisualisations publiques, reproductibles et mises à jour à faible coût.", { size: 31, color: muted, width: wrap(980) }),
      bullets([
        "Données publiques : IPC, HBS et pondérations de consommation",
        "Code disponible sur GitHub, donc vérifiable et réutilisable",
        "Format pensé pour des présentations publiques, policy ou recherche",
      ], { size: 27, dot: amber, width: fixed(940) }),
    ]),
    column({ width: fixed(470), height: fixed(520), padding: { left: 1330, top: 310 }, gap: 16 }, [
      t("2019", { size: 72, bold: true, color: "#9AA6AD" }),
      rule({ width: fixed(310), height: fixed(2), line: { color: "#9AA6AD" } }),
      t("2023", { size: 72, bold: true, color: teal }),
      t("Ecarts mensuels par âge, revenu, lieu de résidence et pays.", { size: 25, color: muted, width: wrap(410) }),
    ]),
  ])
);

addSlide(
  column({ width: fixed(W), height: fixed(H), padding: { left: 120, right: 120, top: 96, bottom: 80 }, gap: 54 }, [
    header("Pourquoi", "L'inflation moyenne ne raconte pas toute l'histoire", "Un même choc de prix ne touche pas les ménages de la même façon."),
    row({ width: fill, height: grow(1), gap: 54, align: "center" }, [
      column({ width: fixed(640), height: fill, justify: "center", gap: 26 }, [
        t("Même choc de prix, paniers différents.", { size: 52, bold: true, width: wrap(610) }),
        t("L'outil transforme cette intuition en graphiques simples, mensuels et comparables.", { size: 30, color: muted, width: wrap(610) }),
      ]),
      bullets([
        "Les paniers de consommation varient selon les ménages",
        "Les écarts deviennent visibles quand les prix bougent vite",
        "La datavisualisation aide à raconter le phénomène sans jargon",
      ], { size: 31, dot: teal }),
    ]),
  ])
);

addSlide(
  column({ width: fixed(W), height: fixed(H), padding: { left: 110, right: 110, top: 76, bottom: 70 }, gap: 42 }, [
    header("L'outil", "Un package R pour passer des données au graphique", "L'objectif est de rendre la mise à jour plus simple, pas d'ajouter une couche technique."),
    row({ width: fill, height: grow(1), gap: 34, align: "center" }, [
      panel({ width: fixed(470), height: fixed(520), padding: { x: 40, y: 42 }, fill: "#F9FAFB", line: { color: "#DDE5EA" }, borderRadius: 8 }, column({ width: fill, height: fill, gap: 22, justify: "center" }, [
        t("1", { size: 58, bold: true, color: teal }),
        t("Télécharger", { size: 36, bold: true }),
        t("Les données publiques de prix et de consommation.", { size: 26, color: muted }),
      ])),
      panel({ width: fixed(470), height: fixed(520), padding: { x: 40, y: 42 }, fill: "#F9FAFB", line: { color: "#DDE5EA" }, borderRadius: 8 }, column({ width: fill, height: fill, gap: 22, justify: "center" }, [
        t("2", { size: 58, bold: true, color: amber }),
        t("Assembler", { size: 36, bold: true }),
        t("Un panier d'inflation par groupe de ménages.", { size: 26, color: muted }),
      ])),
      panel({ width: fixed(470), height: fixed(520), padding: { x: 40, y: 42 }, fill: "#F9FAFB", line: { color: "#DDE5EA" }, borderRadius: 8 }, column({ width: fill, height: fill, gap: 22, justify: "center" }, [
        t("3", { size: 58, bold: true, color: blue }),
        t("Visualiser", { size: 36, bold: true }),
        t("Des graphiques lisibles et prêts à mettre à jour.", { size: 26, color: muted }),
      ])),
    ]),
  ])
);

addSlide(
  column({ width: fixed(W), height: fixed(H), padding: { left: 122, right: 122, top: 92, bottom: 80 }, gap: 58 }, [
    header("Mise à jour", "De l'analyse ponctuelle à l'outil réutilisable", "Le coût de mise à jour baisse fortement quand la chaîne est automatisée."),
    row({ width: fill, height: grow(1), gap: 78, align: "center" }, [
      column({ width: fixed(720), height: fill, justify: "center", gap: 22 }, [
        t("Le vrai gain : refaire proprement, souvent.", { size: 52, bold: true, width: wrap(700) }),
        t("Le package transforme un exercice lourd en routine de suivi.", { size: 30, color: muted, width: wrap(700) }),
      ]),
      bullets([
        "Reproductible : mêmes sources, mêmes choix, mêmes sorties",
        "Actualisable : les prix mensuels peuvent être intégrés vite",
        "Peu coûteux : une relance de script remplace un travail manuel",
      ], { size: 31, dot: blue }),
    ]),
  ])
);

chartSlide({
  label: "France / revenu",
  title: "Inflation par niveau de vie",
  subtitle: "Inflation du 1er quintile, du 5e quintile et moyenne.",
  chartFile: "fig_FR_income_inflation_level3_2021_2026m03.png",
  color: teal,
  bulletsText: [
    "Le graphique montre directement le niveau d'inflation par groupe",
    "Les écarts s'ouvrent surtout quand les prix accélèrent",
  ],
});

chartSlide({
  label: "Lettonie / revenu",
  title: "Inflation par niveau de vie",
  subtitle: "Inflation du 1er quintile, du 5e quintile et moyenne.",
  chartFile: "fig_LV_income_inflation_level_2022_2026m03.png",
  color: purple,
  bulletsText: [
    "La même visualisation peut être reproduite pour un autre pays",
    "Le choc inflationniste est plus marqué dans les pays baltes",
    "La comparaison reste lisible avec la même grammaire graphique",
  ],
});

chartSlide({
  label: "France / âge",
  title: "Inflation par âge",
  subtitle: "Inflation des moins de 30 ans, des 60 ans ou plus et moyenne.",
  chartFile: "fig_FR_age_inflation_level_2021_2026m03.png",
  color: brick,
  bulletsText: [
    "Le niveau d'inflation se lit sans passer par un écart abstrait",
    "Les divergences entre lignes signalent un effet de panier",
    "Le message reste accessible pour un public non technique",
  ],
});

chartSlide({
  label: "France / résidence",
  title: "Inflation par zone de résidence",
  subtitle: "Inflation par zone de résidence, en niveau.",
  chartFile: "fig_FR_urban_inflation_level_2021_2026m03.png",
  color: blue,
  bulletsText: [
    "Les trajectoires rendent visible le rôle du lieu de résidence",
    "La comparaison reste lisible même en présentation grand public",
    "Elle prépare naturellement la décomposition par postes",
  ],
});

chartSlide({
  label: "France / contributions",
  title: "Rural-villes : les postes qui fabriquent l'écart",
  subtitle: "Contribution des postes de consommation à l'écart d'inflation.",
  chartFile: "fig_FR_urban_contribution_gap_2021_2026m03.png",
  color: blue,
  bulletsText: [
    "Les barres décomposent l'écart rural-villes poste par poste",
    "Transport et énergie rendent le territoire immédiatement lisible",
    "La ligne noire résume l'écart total entre les deux groupes",
  ],
  note: "Lecture : un écart positif signifie que le rural subit une inflation plus élevée que les villes.",
});

const hydrationRequests = deck.getPendingImageHydrationRequests();
if (hydrationRequests.length > 0) {
  const payloads = await Promise.all(hydrationRequests.map(async (request) => ({
    assetId: request.assetId,
    contentType: request.contentType,
    data: await readFile(request.uri),
  })));
  deck.hydrateImageAssets(payloads);
}

const pptx = await PresentationFile.exportPptx(deck);
let pptxPath = path.join(outputDir, "inflationinequality_dataviz_tool.pptx");
try {
  await pptx.save(pptxPath);
} catch (error) {
  if (error?.code !== "EBUSY") {
    throw error;
  }
  pptxPath = path.join(outputDir, "inflationinequality_dataviz_tool_revised.pptx");
  await pptx.save(pptxPath);
}

for (const [i, slide] of deck.slides.items.entries()) {
  const png = await slide.export({ format: "png" });
  await writeFile(path.join(previewDir, `slide_${String(i + 1).padStart(2, "0")}.png`), Buffer.from(await png.arrayBuffer()));
}

console.log(`Wrote ${pptxPath}`);
console.log(`Wrote previews to ${previewDir}`);
process.exit(0);
