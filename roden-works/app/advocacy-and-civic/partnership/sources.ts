import type { Source } from '@/components/ui/Sources'

// Order = order of first citation on the page, so footnote numbers read 1, 2, 3… top to bottom.
export const PARTNERSHIP_SOURCES: Source[] = [
  {
    id: 'bptw-samhsa',
    title: 'Ranking Detail: Substance Abuse and Mental Health Services Administration',
    publisher: 'Partnership for Public Service, Best Places to Work in the Federal Government',
    url: 'https://bestplacestowork.org/rankings/detail/?c=HE32',
    note: 'SAMHSA scores, 2020 vs 2022: engagement 37.1 to 74.2; senior leaders 29.2 to 73.8; supervisors 72.2 to 85.0; pay and benefits 67.8 to 73.1.',
  },
  {
    id: 'samhsa-strong',
    title: 'SAMHSA STRONG: Teaming up to transform the workplace',
    publisher: 'Partnership for Public Service',
    url: 'https://ourpublicservice.org/about/history-and-impact/samhsa-strong-teaming-up-to-transform-the-workplace',
    note: 'Partnership began August 2021; score doubled from about 37 to 74, surpassing the 2022 government-wide average.',
  },
  {
    id: 'pps-wiki',
    title: 'Partnership for Public Service',
    publisher: 'Wikipedia',
    url: 'https://en.wikipedia.org/wiki/Partnership_for_Public_Service',
    note: 'Founded 2001 by Samuel J. Heyman with $25 million; Best Places to Work, Service to America Medals, Center for Presidential Transition.',
  },
  {
    id: 'samhsa-about',
    title: 'About Us',
    publisher: 'SAMHSA',
    url: 'https://www.samhsa.gov/about',
    note: 'Agency within HHS; its helplines include the 988 Lifeline and Disaster Distress Helpline, and FindTreatment.gov.',
  },
  {
    id: 'aljazeera-988',
    title: '988: US to launch mental health and suicide prevention hotline',
    publisher: 'Al Jazeera',
    date: 'Jul 15, 2022',
    url: 'https://www.aljazeera.com/amp/news/2022/7/15/988-us-to-launch-mental-health-and-suicide-prevention-hotline',
    note: 'National Suicide Prevention Lifeline moved to 988 on July 16, 2022.',
  },
  {
    id: 'mindsite',
    title: 'Biden Signs New Funding Bill, Boosting Money for Mental Health',
    publisher: 'MindSite News',
    date: 'Mar 15, 2022',
    url: 'https://mindsitenews.org/2022/03/15/fy-2022-funding-bill-boosts-money-for-mental-health-extends-tele-mental-health/',
    note: 'SAMHSA funded at $6.5 billion in FY 2022.',
  },
  {
    id: 'usafacts',
    title: 'Substance Abuse and Mental Health Services Administration',
    publisher: 'USAFacts',
    url: 'https://usafacts.org/explainers/what-does-the-us-government-do/subagency/substance-abuse-and-mental-health-services-administration/',
    note: 'About 527 civilian federal employees (May 2026).',
  },
  {
    id: 'bptw-about',
    title: 'About',
    publisher: 'Partnership for Public Service, Best Places to Work in the Federal Government',
    url: 'https://bestplacestowork.org/about/',
    note: 'Rankings based on OPM\'s Federal Employee Viewpoint Survey; "increased employee engagement leads to better performance and outcomes."',
  },
  {
    id: 'bptw-2021',
    title: 'The Best Places to Work in the Federal Government: 2021 rankings overview',
    publisher: 'Partnership for Public Service and Boston Consulting Group',
    date: 'Jul 2022',
    url: 'https://bestplacestowork.org/wp-content/uploads/sites/11/2022/07/BPTW21_Messaging-One-Pager.pdf',
    note: '2021 rankings include 503 federal agencies and subcomponents.',
  },
]
