import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
// YCOD's founding (2016) and nonprofit status are the owner's own facts and are not cited.
export const YCOD_SOURCES: Source[] = [
  {
    id: 'hrsa-stats',
    title: 'Organ Donation Statistics',
    publisher: 'HRSA, OrganDonor.gov',
    url: 'https://www.organdonor.gov/learn/organ-donation-statistics',
    note: '100,000+ people on the national waiting list; 17 people die each day waiting; another person added every 8 minutes.',
  },
  {
    id: 'wkbw',
    title: 'College freshmen in New York develop plan to encourage more organ donors',
    publisher: 'WKBW (Olivia Proia), syndicated by Scripps stations',
    date: 'Jan 4, 2021',
    url: 'https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors',
    note: 'About 37% of New Yorkers registered, "the lowest opt-in rate of any state or country."',
  },
  {
    id: 'spectrum-redcross',
    title: 'WNY Teens Nominated for American Red Cross Award for Organ Donation Coalition',
    publisher: 'Spectrum News',
    date: '2021',
    url: 'https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition',
    note: "New York's 37% donor registration rate; 2021 American Red Cross Real Heroes Education Award nomination.",
  },
  {
    id: 'cityandstate',
    title: 'Opinion: New York reached a major health milestone, but we cannot take our foot off the gas',
    publisher: 'City & State New York',
    date: 'Mar 7, 2025',
    url: 'https://cityandstateny.com/opinion/2025/03/opinion-new-york-reached-major-health-milestone-we-cannot-take-our-foot-gas/403592',
    note: 'New York passed 50% registered donors in 2024; national average 64%.',
  },
  {
    id: 'hrsa-diversity',
    title: "Let's Talk About Donor Diversity (infographic)",
    publisher: 'HRSA, OrganDonor.gov',
    url: 'https://www.organdonor.gov/sites/default/files/organ-donor/professional/materials/lets-talk-donor-diversity-infographic-english.pdf',
    note: 'People of color are 40% of the U.S. population but 60% of the waiting list (OPTN data).',
  },
  {
    id: 'omh',
    title: 'Organ Transplants and Black/African Americans',
    publisher: 'HHS Office of Minority Health',
    url: 'https://minorityhealth.hhs.gov/organ-transplants-and-blackafrican-americans',
    note: 'Black Americans about 27% of waiting-list candidates and about 12% of organ donors.',
  },
  {
    id: 'a7954',
    title: 'Assembly Bill A7954 (2019-2020): Relates to presumed consent to organ and tissue donation',
    publisher: 'New York State Senate',
    date: 'Introduced May 29, 2019',
    url: 'https://www.nysenate.gov/legislation/bills/2019/A7954',
    note: 'Sponsor Asm. David DiPietro; held in the Assembly Transportation Committee.',
  },
  {
    id: 's4334',
    title: 'Senate Bill S4334 (2021-2022): Presumed consent to organ and tissue donation',
    publisher: 'New York State Senate',
    date: 'Introduced Feb 3, 2021',
    url: 'https://www.nysenate.gov/legislation/bills/2021/S4334',
    note: 'Sponsor Sen. Patrick M. Gallivan.',
  },
  {
    id: 'spectrum-jan2021',
    title: 'College Students Push for More Organ Donations in NY',
    publisher: 'Spectrum News Buffalo',
    date: 'Jan 12, 2021',
    url: 'https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny-',
  },
  {
    id: 'weny',
    title: 'College Activists Pushing For Change to Organ Donor Registration Process in NYS',
    publisher: 'WENY News',
    url: 'https://weny.com/story/43131791/college-activists-pushing-for-change-to-organ-donor-registration-process-in-nys',
  },
  {
    id: 's1594',
    title: 'Senate Bill S1594 (2021-2022): New York State Living Donor Support Act',
    publisher: 'New York State Senate',
    date: 'Signed Dec 29, 2022 (Chapter 814)',
    url: 'https://www.nysenate.gov/legislation/bills/2021/S1594',
    note: 'Same as A146; reimburses living donors for lost wages, travel, lodging, and child care.',
  },
]
