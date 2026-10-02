import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const HAPS_SOURCES: Source[] = [
  {
    id: 'epa-pm',
    title: 'Indoor Particulate Matter',
    publisher: 'U.S. Environmental Protection Agency',
    url: 'https://www.epa.gov/indoor-air-quality-iaq/indoor-particulate-matter',
    note: 'Indoor PM sources (cooking, candles, unvented space heaters, smoking) and outdoor particles that migrate indoors; small particles get deep into the lungs and some reach the bloodstream.',
  },
  {
    id: 'rabito',
    title:
      'The association between short-term residential black carbon concentration on blood pressure in a general population sample',
    publisher: 'Rabito FA, et al., Indoor Air',
    date: '2021',
    url: 'https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/',
    note: '+7.55 mmHg systolic blood pressure per 1 µg/m³ residential black carbon (P = .02); black carbon comes from incomplete combustion and is tied to traffic and cooking.',
  },
  {
    id: 'epa-no2',
    title: "Nitrogen Dioxide's Impact on Indoor Air Quality",
    publisher: 'U.S. Environmental Protection Agency',
    url: 'https://www.epa.gov/indoor-air-quality-iaq/nitrogen-dioxides-impact-indoor-air-quality',
    note: 'Indoor NO₂ from gas stoves and unvented heaters; respiratory irritation and asthma effects; venting reduces exposure.',
  },
  {
    id: 'who-aqg',
    title:
      'WHO global air quality guidelines: particulate matter (PM2.5 and PM10), ozone, nitrogen dioxide, sulfur dioxide and carbon monoxide',
    publisher: 'World Health Organization',
    date: 'Sept 22, 2021',
    url: 'https://www.who.int/publications/i/item/9789240034228',
    note: 'Table 0.1 guideline levels (PM2.5 5/15 µg/m³, NO₂ 10/25 µg/m³); good-practice statements only for black carbon.',
  },
  {
    id: 'lewington',
    title:
      'Age-specific relevance of usual blood pressure to vascular mortality: a meta-analysis of individual data for one million adults in 61 prospective studies',
    publisher: 'Lewington S, et al. (Prospective Studies Collaboration), The Lancet',
    date: 'Dec 14, 2002',
    url: 'https://pubmed.ncbi.nlm.nih.gov/12493255/',
    note: '2 mmHg lower usual systolic BP ≈ 7% lower IHD mortality and ≈ 10% lower stroke mortality.',
  },
]
