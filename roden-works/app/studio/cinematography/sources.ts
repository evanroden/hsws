import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const CINEMATOGRAPHY_SOURCES: Source[] = [
  {
    id: 'claiborne-about',
    title: 'About Us',
    publisher: 'Claiborne Avenue Productions',
    url: 'https://www.claiborneave.com/about-us/',
    note: 'Albert J. Moten, Jr., producer, Claiborne Avenue Productions.',
  },
  {
    id: 'afi-12-years',
    title: '12 Years a Slave (2013)',
    publisher: 'AFI Catalog',
    url: 'https://catalog.afi.com/Catalog/moviedetails/69781',
    note: 'Credits list “Albert Moten, Jr., Loc asst.”',
  },
  {
    id: 'imdb-moten',
    title: 'Albert J. Moten Jr.',
    publisher: 'IMDb',
    url: 'https://www.imdb.com/name/nm4328198/',
    note: 'Credits include Now You See Me (2013) and 12 Years a Slave (2013).',
  },
  {
    id: 'lcm-about',
    title: 'About LCM',
    publisher: "Louisiana Children's Museum",
    url: 'https://lcm.org/about/',
    note: 'New Orleans museum where kids learn through play and shared exploration.',
  },
  {
    id: 'wwno-wiki',
    title: 'WWNO',
    publisher: 'Wikipedia',
    url: 'https://en.wikipedia.org/wiki/WWNO',
    note: 'Public radio station in New Orleans; NPR member.',
  },
  {
    id: 'cined-bmpcc6k',
    title: 'Blackmagic Pocket Cinema Camera 6K Announced – Super 35 Sensor and EF Mount',
    publisher: 'CineD',
    date: 'Aug 8, 2019',
    url: 'https://www.cined.com/blackmagic-pocket-cinema-camera-6k-announced-super-35-sensor-and-ef-mount/',
    note: '6K Super 35 sensor, 13 stops of dynamic range, Blackmagic RAW.',
  },
  {
    id: 'newsshooter-a7sii',
    title: 'IBC 2015: A detailed look at the new Sony a7S II camera',
    publisher: 'Newsshooter',
    date: 'Sept 11, 2015',
    url: 'https://www.newsshooter.com/?p=29057',
    note: 'Full-frame 12.2MP sensor, ISO up to 409,600, S-Log2 and S-Log3.',
  },
  {
    id: 'pvc-cinema-grade',
    title: 'Cinema Grade – a new way to color grade footage inside of your NLE',
    publisher: 'ProVideo Coalition',
    date: 'Sept 19, 2018',
    url: 'https://www.provideocoalition.com/cinema-grade-a-new-way-to-color-grade-footage-inside-of-your-nle/',
    note: 'Color grading plug-in operated directly on the image.',
  },
]
