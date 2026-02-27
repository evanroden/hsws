import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import YcodHero from './YcodHero'
import CrisisDashboard from './CrisisDashboard'
import YcodStory from './YcodStory'
import VideoFeature from './VideoFeature'
import LegislativeTimeline from './LegislativeTimeline'
import MediaWall from './MediaWall'

export const metadata: Metadata = {
  title: 'The YCOD — Opt-Out Organ Donation Advocacy',
  description:
    'The Youth Coalition For Organ Donation — a 501(c)(4) organization reshaping organ donation policy through presumed consent legislation.',
}

export default function YcodPage() {
  return (
    <>
      <BreadcrumbJsonLd
        items={[
          { name: 'Advocacy', href: '/advocacy-and-civic' },
          { name: 'The YCOD' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'The YCOD' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1500} />
      </div>
      <YcodHero />
      <CrisisDashboard />
      <YcodStory />
      <VideoFeature />
      <LegislativeTimeline />
      <MediaWall />
      <ProjectNav currentSlug="ycod" />
    </>
  )
}
