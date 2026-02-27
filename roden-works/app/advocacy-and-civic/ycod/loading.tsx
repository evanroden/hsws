export default function Loading() {
  return (
    <div className="min-h-screen bg-slate-950">
      {/* Breadcrumb skeleton */}
      <div className="content-width pt-24 md:pt-28 pb-4">
        <div className="flex items-center gap-2">
          <div className="skeleton h-4 w-12" />
          <div className="skeleton h-4 w-24" />
          <div className="skeleton h-4 w-16" />
        </div>
      </div>

      {/* Hero skeleton */}
      <div className="content-width py-16">
        <div className="skeleton h-4 w-32 mb-4" />
        <div className="skeleton h-12 w-3/4 mb-4" />
        <div className="skeleton h-6 w-1/2 mb-8" />
      </div>

      {/* Stats skeleton */}
      <div className="content-width py-16">
        <div className="grid grid-cols-2 md:grid-cols-4 gap-6">
          {[1, 2, 3, 4].map((i) => (
            <div key={i} className="skeleton aspect-square rounded-xl" />
          ))}
        </div>
      </div>
    </div>
  )
}
