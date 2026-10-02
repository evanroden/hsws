import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
// The Vogue Italy feature is confirmed by Evan; no public source was found for it (Oct 2026), so it is not cited.
export const MODELING_SOURCES: Source[] = [
  {
    id: 'wivb-stoll',
    title: 'WNY teen shares the runway with big-name models for major fashion companies',
    publisher: 'News 4 Buffalo (WIVB)',
    date: 'July 30, 2019',
    url: 'https://digital-release.wivb.com/news/local-news/erie-county/orchard-park/wny-teen-shares-runway-with-big-name-models-for-major-fashion-companies',
    note: 'Austin Stoll (“Audi”) of Orchard Park, NY: model and designer of the clothing line Bizar.',
  },
]
