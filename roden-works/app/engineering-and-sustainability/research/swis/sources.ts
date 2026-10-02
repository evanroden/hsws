import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
// The wedge-model note cites ids that match the SourceId keys in wedgePhysics.ts.
export const SWIS_SOURCES: Source[] = [
  {
    id: 'pbs',
    title: 'Why the saltwater wedge climbing up the Mississippi River is a wake-up call to the region',
    publisher: 'PBS News',
    date: 'Oct 10, 2023',
    url: 'https://www.pbs.org/newshour/nation/why-salt-water-is-threatening-drinking-water-in-new-orleans-and-what-officials-are-doing-about-it',
    note: 'Close to a million residents in four parishes; river at 150,000 cfs, half of what is needed to keep salt water out; Plaquemines water advisories; Carrollton plant.',
  },
  {
    id: 'wj0929',
    title: 'Corps Augments Underwater Sill To Slow Salt Water Intrusion',
    publisher: 'The Waterways Journal',
    date: 'Sept 29, 2023',
    url: 'https://www.waterwaysjournal.net/2023/09/29/corps-augments-underwater-sill-to-slow-salt-water-intrusion/',
    note: 'Intrusion below 300,000 cfs; July sill; overtopped around Sept 20; raised to −30 ft from Sept 24 with a 620-ft notch; toe at RM 69.4 on Sept 27.',
  },
  {
    id: 'dvids',
    title:
      'USACE New Orleans District team receives 2024 Innovation Award for notched sill barrier used to arrest saltwater intrusion up Mississippi River',
    publisher: 'U.S. Army Corps of Engineers, New Orleans District (DVIDS)',
    date: 'Sept 24, 2024',
    url: 'https://www.dvidshub.net/news/481623',
    note: 'Belle Chasse RM 75.5, Carrollton RM 104.7; sill −55 → −30 ft; 2023 toe RM 69.4 (surface RM 54.4); projected RM 103.7 (surface RM 88.7) with the original sill.',
  },
  {
    id: 'fox8',
    title: 'Significant adjustment in saltwater wedge timeline delays some impacts by nearly a month, if at all',
    publisher: 'FOX 8 WVUE',
    date: 'Oct 5, 2023',
    url: 'https://www.fox8live.com/2023/10/05/significant-adjustment-saltwater-wedge-timeline-delays-some-impacts-by-nearly-month-if-all/',
    note: 'Corps intake river miles: Belle Chasse, Dalcour, St. Bernard, Algiers, Carrollton.',
  },
  {
    id: 'noaa',
    title: 'United States Coast Pilot 5, Chapter 8: Mississippi River',
    publisher: 'NOAA Office of Coast Survey',
    url: 'https://nauticalcharts.noaa.gov/publications/coast-pilot/files/cp5/CPB5_C08_WEB.pdf',
    note: 'River miles above Head of Passes and channel depths.',
  },
  {
    id: 'csm',
    title: "Saltwater influx tests communities near Mississippi's mouth",
    publisher: 'The Christian Science Monitor',
    date: 'Oct 23, 2023',
    url: 'https://www.csmonitor.com/Environment/2023/1023/Saltwater-influx-tests-communities-near-Mississippi-s-mouth',
    note: 'Corps: 300,000 cfs is "the magic number"; the RM 64 sill raised the bed nearly 35 ft.',
  },
  {
    id: 'nbc',
    title: 'New Orleans braces for drinking water emergency from drought-stricken Mississippi River',
    publisher: 'NBC News',
    date: 'Sept 26, 2023',
    url: 'https://www.nbcnews.com/science/environment/new-orleans-braces-drinking-water-emergency-drought-stricken-mississip-rcna117218',
    note: 'Flow of 148,000 cfs the week the wedge overtopped the sill.',
  },
  {
    id: 'wwno0919',
    title: 'Salt water threatens South Louisiana drinking water second year in a row amid severe drought',
    publisher: 'WWNO',
    date: 'Sept 19, 2023',
    url: 'https://www.wwno.org/coastal-desk/2023-09-19/saltwater-threatens-south-louisiana-drinking-water-second-year-in-a-row-amid-severe-drought',
    note: 'Flow forecast to drop to 130,000 cfs by mid-October; 1988 record low of 120,000 cfs let the wedge reach Kenner.',
  },
  {
    id: 'nps',
    title: 'Mississippi River Facts',
    publisher: 'National Park Service',
    url: 'https://www.nps.gov/miss/riverfacts.htm',
    note: 'Average flow at New Orleans: 600,000 cfs.',
  },
  {
    id: 'gohsep',
    title: 'USACE to construct underwater sill to arrest saltwater progression into Mississippi River',
    publisher: 'Governor’s Office of Homeland Security and Emergency Preparedness (USACE release)',
    date: 'Aug 29, 2024',
    url: 'https://gohsep.la.gov/about/news/usace-to-construct-underwater-sill-to-arrest-saltwater-progression-into-mississippi-river/',
    note: 'Sills at River Mile 64 near Myrtle Grove in 1988, 1999, 2012, 2022 and 2023.',
  },
]
