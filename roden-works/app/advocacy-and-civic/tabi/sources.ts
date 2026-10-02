import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const TABI_SOURCES: Source[] = [
  {
    id: 'aurora-wiki',
    title: 'Aurora, Erie County, New York',
    publisher: 'Wikipedia',
    url: 'https://en.wikipedia.org/wiki/Aurora,_Erie_County,_New_York',
    note: 'Town in Erie County, Western New York; contains the Village of East Aurora.',
  },
  {
    id: 'fcc-benchmark',
    title: 'FCC Increases Broadband Benchmark',
    publisher: 'Broadband Breakfast',
    date: 'Mar 14, 2024',
    url: 'https://broadbandbreakfast.com/fcc-increases-broadband-benchmark/',
    note: '25/3 Mbps benchmark from 2015 raised to 100/20 Mbps.',
  },
  {
    id: 'benton',
    title: 'How the FCC Got to 100/20',
    publisher: 'Benton Institute for Broadband & Society',
    date: 'Mar 14, 2024',
    url: 'https://benton.org/blog/how-fcc-got-10020',
    note: 'IIJA (2021): no 25/3 access = unserved; no 100/20 access = underserved.',
  },
  {
    id: 'wkbw-erienet',
    title: "'We want to be an example': ErieNet plans taking shape, 400 miles of fiber cable to be installed next month",
    publisher: 'WKBW',
    date: 'Mar 19, 2024',
    url: 'https://www.wkbw.com/news/local-news/buffalo/erienet-plans-taking-shape-400-miles-of-fiber-cable-to-be-installed-next-month',
    note: '400 miles of open-access fiber; $36 million in American Rescue Plan funds; unserved and underserved areas.',
  },
]
