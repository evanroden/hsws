import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const TED_SOURCES: Source[] = [
  {
    id: 'ted-talk',
    title: 'The Myth of the Apolitical Youth',
    publisher: 'TEDxTulane',
    date: 'Mar 2022',
    url: 'https://www.ted.com/talks/evan_roden_the_myth_of_the_apolitical_youth',
  },
  {
    id: 'red-cross-nomination',
    title: 'WNY Teens Nominated for American Red Cross Award for Organ Donation Coalition',
    publisher: 'Spectrum News',
    date: '2021',
    url: 'https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition',
    note: 'YCOD’s 3,000 members across the US and abroad; opt-out bill awaiting action in New York.',
  },
  {
    id: 'ipu-youth-2026',
    title: 'Youth representation in parliament flatlines for the first time in 12 years',
    publisher: 'Inter-Parliamentary Union',
    date: 'Apr 2026',
    url: 'https://www.ipu.org/news/press-releases/2026-04/youth-representation-in-parliament-flatlines-first-time-in-12-years',
    note: 'Half the world’s population is under 30; 2.8% of MPs are aged 30 or under.',
  },
  {
    id: 'amendment-26',
    title: 'The Constitution: Amendments 11–27 (Amendment XXVI)',
    publisher: 'U.S. National Archives',
    date: 'Ratified July 1, 1971',
    url: 'https://www.archives.gov/founding-docs/amendments-11-27',
    note: 'Sets 18 as the U.S. voting age.',
  },
]
