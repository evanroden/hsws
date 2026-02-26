import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import EnfraHero from './EnfraHero'
import EnfraOverview from './EnfraOverview'
import EnergyPlantDiagram from './EnergyPlantDiagram'
import SavingsVisualization from './SavingsVisualization'
import FacilityMap from './FacilityMap'

export const metadata: Metadata = {
  title: 'ENFRA × Rochester Regional Health',
  description:
    '$143.8 million, 30-year Energy-as-a-Service partnership delivering $354.6M in guaranteed savings.',
}

export default function EnfraPage() {
  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'ENFRA' },
        ]}
      />
      <EnfraHero />
      <EnfraOverview />
      <EnergyPlantDiagram />
      <SavingsVisualization />
      <FacilityMap />
    </>
  )
}
