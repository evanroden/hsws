import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const WIMLEY_SOURCES: Source[] = [
  {
    id: 'wimley-faculty',
    title: 'William C. Wimley, PhD',
    publisher: 'Tulane University School of Medicine',
    url: 'https://medicine.tulane.edu/departments/biochemistry-molecular-biology-tulane-cancer-center/faculty/william-c-wimley-phd',
    note: 'George A. Adrouny Endowed Professor, Department of Biochemistry and Molecular Biology.',
  },
  {
    id: 'phd2017',
    title: 'pH-Triggered, Macromolecule-Sized Poration of Lipid Bilayers by Synthetically Evolved Peptides',
    publisher: 'Wiedman G, et al., Journal of the American Chemical Society',
    date: 'Jan 2017',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC5521809/',
    note: 'pHD peptides: little activity at physiological pH, macromolecule-sized pores at acidic pH (apparent pKa 5.5–5.8); proposed for cargo release and cancer therapeutics.',
  },
  {
    id: 'macrolittins2018',
    title:
      'Potent Macromolecule-Sized Poration of Lipid Bilayers by the Macrolittins, A Synthetically Evolved Family of Pore-Forming Peptides',
    publisher: 'Li S, et al., Journal of the American Chemical Society',
    date: 'May 2018',
    url: 'https://pubmed.ncbi.nlm.nih.gov/29694775/',
    note: 'Macrolittins release macromolecules from PC vesicles at neutral pH at peptide-to-lipid ratios as low as 1:1000.',
  },
  {
    id: 'bee-venom',
    title: 'Bee Venom and Its Two Main Components—Melittin and Phospholipase A2—As Promising Antiviral Drug Candidates',
    publisher: 'Yaacoub C, et al., Pathogens',
    date: 'Nov 2023',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC10674158/',
  },
  {
    id: 'tulane-sme',
    title: 'Synthetic Molecular Evolution of Peptides',
    publisher: 'Wimley Lab, Tulane School of Medicine',
    url: 'https://medicine.tulane.edu/wimley-lab/synthetic-molecular-evolution-peptides',
    note: 'Iterative design and screening of combinatorial peptide libraries.',
  },
  {
    id: 'starr2020',
    title:
      'Synthetic molecular evolution of host cell-compatible, antimicrobial peptides effective against drug-resistant, biofilm-forming bacteria',
    publisher: 'Starr CG, et al., PNAS',
    date: 'Apr 2020',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC7165445/',
    note: 'Peptides stay active in concentrated human blood cells, where typical antimicrobial peptides lose activity; no measurable resistance over 10 passages.',
  },
  {
    id: 'tulane-pore',
    title: 'Pore-Forming Peptides',
    publisher: 'Wimley Lab, Tulane School of Medicine',
    url: 'https://medicine.tulane.edu/wimley-lab/pore-forming-peptides',
    note: 'Collaboration with the Hristova Lab at Johns Hopkins.',
  },
  {
    id: 'acsnano2024',
    title: 'Structural Determinants of Peptide Nanopore Formation',
    publisher: 'Sun L, Hristova K, Bondar A-N, Wimley WC, ACS Nano',
    date: '2024',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC11191747/',
    note: 'Macrolittins evolved from melittin via MelP5; nanopores at very low concentration with essentially no cytolytic activity against cell membranes.',
  },
  {
    id: 'melp5-2014',
    title: 'Highly efficient macromolecule-sized poration of lipid bilayers by a synthetically evolved peptide',
    publisher: 'Wiedman G, et al., Journal of the American Chemical Society',
    date: 'Mar 2014',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC3985440/',
    note: 'MelP5, a gain-of-function melittin variant, forms equilibrium pores that pass macromolecules.',
  },
  {
    id: 'tumor-acid',
    title: 'The Acidic Tumor Microenvironment as a Driver of Cancer',
    publisher: 'Boedtkjer E, Pedersen SF, Annual Review of Physiology',
    date: 'Feb 2020',
    url: 'https://pubmed.ncbi.nlm.nih.gov/31730395/',
  },
  {
    id: 'nanopore-detect',
    title: 'De novo design of a nanopore for single-molecule detection that incorporates a β-hairpin peptide',
    publisher: 'Shimizu K, et al., Nature Nanotechnology',
    date: 'Jan 2022',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC8770118/',
    note: 'Self-assembling peptide nanopores used for single-molecule detection of DNA and polypeptides.',
  },
]
