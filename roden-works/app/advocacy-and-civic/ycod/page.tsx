import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd, ArticleJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import YcodHero from './YcodHero'
import CrisisDashboard from './CrisisDashboard'
import YcodStory from './YcodStory'
import VideoFeature from './VideoFeature'
import LegislativeTimeline from './LegislativeTimeline'
import MediaWall from './MediaWall'

export const metadata: Metadata = {
  title: 'The YCOD: Opt-Out Organ Donation Advocacy',
  description:
    // Nonprofit per IRS CP 575 E EIN notice (Feb 3, 2022) provided by Evan. An EIN does not confer
    // 501(c) tax-exempt status, so no 501(c)(3)/(4) claim is made.
    'The Youth Coalition For Organ Donation is a youth-led nonprofit working to pass presumed consent organ donation legislation in New York.',
}

export default function YcodPage() {
  return (
    <>
      <ArticleJsonLd
        title="The YCOD: Opt-Out Organ Donation Advocacy"
        description="The Youth Coalition For Organ Donation is a youth-led coalition working to pass presumed consent organ donation legislation in New York."
        path="/advocacy-and-civic/ycod"
      />
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
