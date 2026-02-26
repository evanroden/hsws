import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import YcodHero from './YcodHero'
import CrisisDashboard from './CrisisDashboard'
import YcodStory from './YcodStory'
import LegislativeTimeline from './LegislativeTimeline'
import MediaWall from './MediaWall'

export const metadata: Metadata = {
  title: 'The YCOD — Organ Donation Advocacy',
  description:
    'The Youth Coalition For Organ Donation — a 501(c)(4) organization reshaping organ donation policy through presumed consent legislation.',
}

export default function YcodPage() {
  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'The YCOD' },
        ]}
      />
      <YcodHero />
      <CrisisDashboard />
      <YcodStory />
      <LegislativeTimeline />
      <MediaWall />
    </>
  )
}
