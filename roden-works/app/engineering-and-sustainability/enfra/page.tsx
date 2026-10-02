import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd, ArticleJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import EnfraHero from './EnfraHero'
import EnfraOverview from './EnfraOverview'
import EnergyPlantDiagram from './EnergyPlantDiagram'
import SavingsVisualization from './SavingsVisualization'
import FacilityMap from './FacilityMap'
import { SourceList } from '@/components/ui/Sources'
import { ENFRA_SOURCES } from './sources'

export const metadata: Metadata = {
  title: 'ENFRA × Rochester Regional Health — $143.8M EaaS Partnership',
  // Source: https://enfrasolutions.com/enfra-and-rochester-regional-health-launch-30-year-energy-as-a-service-partnership-to-modernize-system-wide-infrastructure-and-advance-sustainability
  description:
    '$143.8 million, 30-year Energy-as-a-Service partnership with 34.4% guaranteed savings, equal to more than $354.6M in avoided costs.',
}

export default function EnfraPage() {
  return (
    <>
      <ArticleJsonLd
        title="ENFRA × Rochester Regional Health — $143.8M EaaS Partnership"
        description="$143.8 million, 30-year Energy-as-a-Service partnership with 34.4% guaranteed savings, equal to more than $354.6M in avoided costs."
        path="/engineering-and-sustainability/enfra"
      />
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'ENFRA' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'ENFRA' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1800} />
      </div>
      <EnfraHero />
      <EnfraOverview />
      <EnergyPlantDiagram />
      <SavingsVisualization />
      <FacilityMap />
      <SourceList sources={ENFRA_SOURCES} />
      <ProjectNav currentSlug="enfra" />
    </>
  )
}
