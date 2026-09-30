/**
 * Chart tokens — the single source for every chart and diagram on the site.
 *
 * The three series hues were validated as a categorical set with the dataviz
 * palette validator (dark mode, all-pairs, on both #151C1F and #0B1215):
 * lightness band, chroma floor, CVD ΔE 9.9 (deutan), normal-vision ΔE 17.6,
 * contrast ≥ 3:1. Assign them in this fixed order — never cycle or add hues.
 */
export const chart = {
  surface: '#151C1F',
  series: ['#B87333', '#3DA887', '#6A8CDB'] as const, // copper, verdigris, steel
  copper: '#B87333',
  verdigris: '#3DA887',
  steel: '#6A8CDB',
  /** De-emphasis mark ("the rest" in an emphasis chart) — 3.2:1 on surface */
  deemph: '#5E6C76',
  /** Hairline gridline, one step off the surface */
  grid: '#232B30',
  /** Baseline / axis rule */
  axis: '#3A454D',
  text: {
    primary: '#F2F5F7',
    secondary: '#A9B6BF',
    muted: '#7E8F9B',
  },
  /** Area washes use the series hue at this opacity */
  areaOpacity: 0.12,
} as const

export type SeriesColor = (typeof chart.series)[number]
