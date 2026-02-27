import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import EnfraHero from './EnfraHero'
import EnfraOverview from './EnfraOverview'
import EnergyPlantDiagram from './EnergyPlantDiagram'
import SavingsVisualization from './SavingsVisualization'
import FacilityMap from './FacilityMap'

export const metadata: Metadata = {
  title: 'ENFRA × Rochester Regional Health — $143.8M EaaS Partnership',
  description:
    '$143.8 million, 30-year Energy-as-a-Service partnership delivering $354.6M in guaranteed savings.',
}

export default function EnfraPage() {
  return (
    <>
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
      <ProjectNav currentSlug="enfra" />
    </>
  )
}
