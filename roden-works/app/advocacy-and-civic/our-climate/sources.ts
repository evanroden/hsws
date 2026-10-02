import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const OUR_CLIMATE_SOURCES: Source[] = [
  {
    id: 'causeiq',
    title: 'Our Climate',
    publisher: 'Cause IQ',
    url: 'https://causeiq.com/organizations/our-climate,464237362',
    note: 'Nonprofit (501(c)(4)) that trains young people to advocate for equitable climate policy.',
  },
  {
    id: 'or-eo',
    title: 'Republican Walkout Halts Cap-and-Invest (Again), but Gov. Brown Commits to Climate',
    publisher: 'Climate XChange',
    date: 'Mar 13, 2020',
    url: 'https://climate-xchange.org/2020/03/republican-walkout-halts-cap-and-invest-again-but-gov-brown-commits-to-climate/',
    note: 'Executive Order 20-04 signed March 10, 2020 after the SB 1530 walkout; 45% below 1990 by 2035, 80% by 2050.',
  },
  {
    id: 'ny-clcpa',
    title: 'NY governor signs into law most ambitious climate plan in the US',
    publisher: 'Al Jazeera',
    date: 'Jul 18, 2019',
    url: 'https://www.aljazeera.com/amp/economy/2019/7/18/ny-governor-signs-into-law-most-ambitious-climate-plan-in-the-us',
    note: 'CLCPA signed July 2019; 70% renewable electricity by 2030; 85% cut with the rest offset (net zero) by 2050.',
  },
  {
    id: 'ma-roadmap',
    title: 'Massachusetts Enacts Major Climate Change Legislation',
    publisher: 'Day Pitney',
    date: 'Mar 30, 2021',
    url: 'https://daypitney.com/insights/publications/2021/03/30-massachusetts-enacts-major-climate-change-leg',
    note: 'Next-Generation Roadmap law signed March 26, 2021, from the 2019-2020 session; net-zero emissions by 2050.',
  },
  {
    id: 'npr-tiles',
    title: "Let's tesselate: Hexagons for tile grid maps",
    publisher: 'NPR Visuals',
    date: 'May 11, 2015',
    url: 'https://blog.apps.npr.org/2015/05/11/hex-tile-maps',
    note: 'Square tile grid map layout used for the state map.',
  },
]
